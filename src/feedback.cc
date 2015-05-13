 /* Copyright (C) 1999 - 2002 Anders Holst
  *
  * This program is free software; you can redistribute it and/or
  * modify it under the terms of the GNU General Public License as
  * published by the Free Software Foundation; either version 2, or
  * (at your option) any later version.
  * 
  * This program is distributed in the hope that it will be useful,
  * but WITHOUT ANY WARRANTY; without even the implied warranty of
  * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  * GNU General Public License for more details.
  * 
  * You should have received a copy of the GNU General Public License
  * along with this software; see the file COPYING.  If not, write to
  * the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
  * Boston, MA 02111-1307 USA
  */

 /*****************************\
 * 		               *
 * User interaction feedback   *
 * 		               *
 \*****************************/

#include <guile/gh.h>

#include "gwm.hh"
#include "gwmfunc.hh"
#include "error.hh"
#include "cursor.hh"
#include "deco.hh"
#include "event.hh"
#include "fsm.hh"
#include "screen.hh"
#include "client.hh"
#include "drawing.hh"

struct clean_grab_data
{
  Decoration* deco;
  int rubber;
  int no_freeze;
};

SCM cleanup_after_grab(void* data)
{
  if (((clean_grab_data*) data)->rubber && GWM_rubber_feedback) {
    EraseRubber();
    GWM_rubber_feedback = 0;
    if (((clean_grab_data*) data)->no_freeze)
      Fsm::TempFreezeOff();
  }
  ((clean_grab_data*) data)->deco->remove_active_grab(0);
  return SCM_UNSPECIFIED;
}

SCM_DEFINE(with_grabbed_server, "with-grabbed-server", 2, 0, 1,
           (SCM deco, SCM thunk, SCM args),
           "Run thunk with the X server grabbed, and events redirected to deco."
           "Keys are:"
           ":grab-keyboard"
           ":confine-pointer"
           ":grab-children"
           ":cursor")
{
  SCM sub, ctx;
  SCM cursor;
  SCM res;
  SCM kbddef;
  Decoration* d;
  int kbd, cnf, chld;
  int n, cn;
  clean_grab_data cldata;
  n = wl_separate_context(args, sub, ctx, cn, s_with_grabbed_server);
  if (n)
    gwm_wrong_num_args(s_with_grabbed_server, n+2);
  must_be_valid_deco(s_with_grabbed_server, deco, 1);
  if (!gh_procedure_p(thunk))
    gwm_wrong_type_arg(s_with_grabbed_server, 2, thunk, "procedure");
  d = WL_DECO(deco);
  kbddef = (d->MaybeMenu()->GetFsm()->CanUseFocus() ? SCM_BOOL_T : SCM_BOOL_F);
  kbd = wl_getbool(gwm_get_keyword(k_grab_keyboard, ctx, cn, kbddef), s_with_grabbed_server);
  cnf = wl_getbool(gwm_get_keyword(k_confine_pointer, ctx, cn, SCM_BOOL_F), s_with_grabbed_server);
  chld = wl_getbool(gwm_get_keyword(k_grab_children, ctx, cn, SCM_BOOL_F), s_with_grabbed_server);
  cursor = gwm_get_keyword(k_cursor, ctx, cn, WL_DECO(deco)->get_cursor());
  d->set_active_grab(0, (WLCURSORP(cursor) ? WL_CURSOR(cursor) : 0),
                     (unsigned int) 0, kbd, chld, cnf, 0, 0);
  cldata.deco = WL_DECO(deco);
  cldata.rubber = 0;
  cldata.no_freeze = 0;
  res = gwm_apply_with_cleanup(thunk, SCM_UNDEFINED, cleanup_after_grab, &cldata);
  cleanup_after_grab(&cldata);
  return res;
}

static int max_freeze_time = 200;

//  (with-user-feedback do-thunk stop-thunk)
//  (with-user-feedback do-thunk event)

SCM_DEFINE(with_user_feedback, "with-user-feedback", 2, 0, 1,
           (SCM thunk, SCM st_arg, SCM args),
           "(with-user-feedback thunk event [key val] ...)"
           "(with-user-feedback thunk stop-thunk [key val] ...)"
           "For each motion, press or release event, run thunk with that event"
           "as argument. If an event is given as argument to 'with-user-feedback,"
           "the first call to thunk is with that event, and the loop stops when"
           "a matching release (or another press) occurs. If a stop-thunk is"
           "given instead, the loop continues until the stop-thunk returns true"
           "for a received event. In the first case the return value is the last"
           "event, in the second case the return value is the result of"
           "stop-thunk. The server will be grabbed during the loop, unless the"
           "key :no-freeze is set to true. Rubber drawings made by the thunks"
           "are shown until the next event. Keys are:"
           ":no-freeze"
           ":cursor")
{
  SCM sub, ctx;
  SCM cursor;
  SCM res, ev;
  int n, cn;
  int ok, first, stproc, nofrz, drawn, button;
  unsigned int ptrmask;
  Window root, sub_window;
  int root_x, root_y, cur_x, cur_y;
  int rootind;
  Decoration* scr;
  XEvent but_ev;
  XEvent* rel_ev = 0;
  clean_grab_data cldata;
  n = wl_separate_context(args, sub, ctx, cn, s_with_user_feedback);
  if (n)
    gwm_wrong_num_args(s_with_user_feedback, n+2);
  if (!gh_procedure_p(thunk))
    gwm_wrong_type_arg(s_with_user_feedback, 1, thunk, "procedure");
  if (gh_procedure_p(st_arg)) {
    stproc = 1;
    ev = SCM_BOOL_F;
    button = 0;
  } else if (WLEVENTP(st_arg)) {
    stproc = 0;
    ev = st_arg;
    if (WL_EVENT(ev)->type() == ButtonPress)
      button = WL_EVENT(ev)->event()->xbutton.button;
    else
      button = 0;
  } else
    gwm_wrong_type_arg(s_with_user_feedback, 3, st_arg, "procedure or xevent");
  nofrz = wl_getbool(gwm_get_keyword(k_no_freeze, ctx, cn, SCM_BOOL_F), s_with_user_feedback);
  cursor = gwm_get_keyword(k_cursor, ctx, cn, SCM_BOOL_F);
  root=0;
  XQueryPointer(dpy,
                Context->Root(), &root, &sub_window,
                &root_x, &root_y, &cur_x, &cur_y, &ptrmask);
  if (root &&
      (rootind = ScreenOfRoot(root)) >= 0 &&
      GWMManagedScreens[rootind])
    scr = GWMManagedScreens[rootind]->Deco();
  else {
    gwm_misc_error(s_with_user_feedback, "Pointer is on an unmanaged screen: ~S\n", gh_int2scm(root ? rootind : -1));
  }
  if (scr->Valid() <= 0)
    gwm_misc_error(s_with_user_feedback, "Screen is not opened: ~A\n", scr->scm());
  scr->set_active_grab(0, (WLCURSORP(cursor) ? WL_CURSOR(cursor) : 0),
                       ButtonPressMask | ButtonReleaseMask | PointerMotionMask,
                       (unsigned int) 0, 1, 0, nofrz, 0);
  cldata.deco = scr;
  cldata.rubber = 1;
  cldata.no_freeze = nofrz;
  ClearRubber();
  GWM_rubber_feedback = 0;
  drawn = 0;
  ok = 1;
  first = 1;
  ptrmask = 0;
  while (ok) {
    gwm_apply_with_cleanup(thunk, ev, cleanup_after_grab, &cldata);
    if (drawn) {
      EraseRubber();
      GWM_rubber_feedback = 0;
      if (nofrz) {
        Fsm::TempFreezeOff();
        XSync(dpy, 0);
//        process_masked_events(ExposureMask);
      }
    }
    drawn = AnythingToDraw();
    if (drawn) {
      if (nofrz)
        Fsm::TempFreezeOn();
      DrawRubber();
      GWM_rubber_feedback = 1;
    }
    if (first && (rel_ev = CheckAnyRelease())) {
      ev = event2scm(new WlEvent(rel_ev, 0));
    } else if (drawn && nofrz) {
      ev = GWM_AwaitPointerEvent(max_freeze_time);
      while (ev == SCM_BOOL_F) {
        UnDrawRubber();
        Fsm::TempFreezeOff();
        XSync(dpy, 0);
//        process_masked_events(ExposureMask);
        Fsm::TempFreezeOn();
        ReDrawRubber();
        ev = GWM_AwaitPointerEvent(max_freeze_time);
      }
    } else
      ev = GWM_AwaitPointerEvent(-1);
    if (!rel_ev)
      first = 0;
    if (stproc) {
      if (WL_EVENT(ev)->type() == ButtonRelease) {
        if (ptrmask & (Button1Mask << (WL_EVENT(ev)->event()->xbutton.button - Button1)))
          ptrmask &= ~(Button1Mask << (WL_EVENT(ev)->event()->xbutton.button - Button1));
        else
          RegisterReleaseRedir(WL_EVENT(ev)->event());
      } else if (WL_EVENT(ev)->type() == ButtonPress) {
        ptrmask |= (Button1Mask << (WL_EVENT(ev)->event()->xbutton.button - Button1));
      }
      res = gwm_apply_with_cleanup(st_arg, ev, cleanup_after_grab, &cldata);
      ok = (res == SCM_BOOL_F);
    } else {
      ok = (WL_EVENT(ev)->type() == MotionNotify);
      if (WL_EVENT(ev)->type() == ButtonRelease)
        RegisterReleaseRedir(WL_EVENT(ev)->event());
      else if (WL_EVENT(ev)->type() == ButtonPress) {
        WaitForButtonRelease(WL_EVENT(ev)->event()->xbutton.button, scr->Xwin(), &but_ev, 0, 0);
        if (button != WL_EVENT(ev)->event()->xbutton.button)
          WaitForButtonRelease(button, scr->Xwin(), &but_ev, 0, 0);
        RegisterReleaseRedir(&but_ev);
      }
    }
  }
  if (stproc && ptrmask) {
    if (GWM_rubber_feedback)
      EraseRubber();
    for (button=Button1; button<=Button5; button++)
      if (ptrmask & (Button1Mask << (button - Button1)))
        WaitForButtonRelease(button, scr->Xwin(), &but_ev, 0, 0);
  }
  cleanup_after_grab(&cldata);
  return (stproc ? res : ev);
}

//  (with-timer-feedback do-thunk delay count)
//  (with-timer-feedback do-thunk del-thunk)

SCM_DEFINE(with_timer_feedback, "with-timer-feedback", 2, 0, 1,
           (SCM thunk, SCM del, SCM args),
           "(with-timer-feedback thunk delay times [key val] ...)"
           "(with-timer-feedback thunk delay-thunk [key val] ...)"
           "Run thunk a number of times with delays between. If delay and times"
           "are given, run it that many times with that delay. If delay-thunk is"
           "given, the delay is given by the return of this thunk, and the loop"
           "stops when the thunk return false (or something that is not a"
           "number). The server will be grabbed during the loop, unless the"
           "key :no-freeze is set to true. Rubber drawings made by the thunks"
           "are shown until the next call to them. Keys are:"
           ":no-freeze"
           ":cursor")
{
  SCM sub, ctx;
  SCM cursor;
  SCM res;
  Decoration* scr;
  int n, cn;
  int delproc, count, rest, start, end;
  int nofrz, drawn;
  clean_grab_data cldata;
  n = wl_separate_context(args, sub, ctx, cn, s_with_timer_feedback);
  if (n != (gh_procedure_p(del) ? 0 : 1))
    gwm_wrong_num_args(s_with_timer_feedback, n+2);
  if (!gh_procedure_p(thunk))
    gwm_wrong_type_arg(s_with_timer_feedback, 1, thunk, "procedure");
  if (gh_procedure_p(del)) {
    delproc = 1;
    count = 1;
  } else {
    if (!gh_number_p(del))
      gwm_wrong_type_arg(s_with_timer_feedback, 2, del, "number");
    must_be_number(s_with_timer_feedback, SCM_CAR(sub), 3);
    delproc = 0;
    count = gh_scm2int(SCM_CAR(sub));
  }
  nofrz = wl_getbool(gwm_get_keyword(k_no_freeze, ctx, cn, SCM_BOOL_F), s_with_timer_feedback);
  cursor = gwm_get_keyword(k_cursor, ctx, cn, SCM_BOOL_F);
  scr = Context->Deco();
  if (scr->Valid() <= 0)
    gwm_misc_error(s_with_timer_feedback, "Screen is not opened: ~A\n", scr->scm());
  scr->set_active_grab(0,
                       (WLCURSORP(cursor) ? WL_CURSOR(cursor) : 0),
                       (unsigned int) 0x8000, // dummy mask != 0
                       0, 0, 0, nofrz, 0);
  cldata.deco = Context->Deco();
  cldata.rubber = 1;
  cldata.no_freeze = nofrz;
  ClearRubber();
  GWM_rubber_feedback = 0;
  drawn = 0;
  while (count > 0) {
    start = gwm_proper_time();
    gwm_apply_with_cleanup(thunk, SCM_UNDEFINED, cleanup_after_grab, &cldata);
    if (delproc)
      res = gwm_apply_with_cleanup(del, SCM_UNDEFINED, cleanup_after_grab, &cldata);
    if (drawn) {
      EraseRubber();
      GWM_rubber_feedback = 0;
      if (nofrz) {
        Fsm::TempFreezeOff();
        XSync(dpy, 0);
//        process_masked_events(ExposureMask);
      }
    }
    drawn = AnythingToDraw();
    if (drawn) {
      if (nofrz)
        Fsm::TempFreezeOn();
      DrawRubber();
      GWM_rubber_feedback = 1;
    }
    if (delproc) {
      if (gh_number_p(res))
        end = start + (int) (gh_scm2double(res)*1000);
      else 
        count = 0;
    } else {
      end = start + (int) (gh_scm2double(del)*1000);
      count--;
    }
    if (count > 0) {
      rest = end - gwm_proper_time();
      if (drawn && nofrz) {
        while (rest > max_freeze_time) {
          GWM_InternSleep(max_freeze_time);
          UnDrawRubber();
          Fsm::TempFreezeOff();
          XSync(dpy, 0);
//          process_masked_events(ExposureMask);
          Fsm::TempFreezeOn();
          ReDrawRubber();
          rest = end - gwm_proper_time();
        }
        GWM_InternSleep(rest);
      } else
        GWM_InternSleep(rest);
    }
  }
  cleanup_after_grab(&cldata);
  return SCM_UNSPECIFIED;
}

// (with-grab-feedback deco stop-thunk)

void init_scm_feedback()
{
#include "feedback.x"
}
