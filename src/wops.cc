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


/**************************************\
* 				       *
* 	various window operations      *
* 				       *
\**************************************/

/*  include  */

#include <guile/gh.h>

#include "gwm.hh"
#include "gwmfunc.hh"
#include "error.hh"
#include "deco.hh"
#include "event.hh"
#include "fsm.hh"
#include "paint.hh"
#include "screen.hh"
#include "client.hh"
#include "wops.hh"

/*  routines  */

SCM_DEFINE(get_deco_icon, "icon-deco", 1, 0, 0,
           (SCM win),
           "Get the icon decoration corresponding to the given window or icon deco.")
{
  Decoration* deco;
  must_be_valid_deco(s_get_deco_icon, win, 1);
  deco = WL_DECO(win); 
  if (!deco->Win())
    return SCM_BOOL_F;
  if (!(deco->Win()->Icon()))
    if (!RealizeIconWindow(deco->Win()->Inner(), 0))
      return SCM_BOOL_F;
  return deco->Win()->Icon()->scm();
}

SCM_DEFINE(deco_icon_exists, "icon-decorated?", 1, 0, 0,
           (SCM win),
           "Check if the icon decoration for the client is realized yet.")
{
  Decoration* deco;
  must_be_valid_deco(s_deco_icon_exists, win, 1);
  deco = WL_DECO(win); 
  if (!deco->Win())
    return SCM_BOOL_F;
  if (!(deco->Win()->Icon()))
    return SCM_BOOL_F;
  else
    return SCM_BOOL_T;
}

SCM_DEFINE(get_deco_window, "window-deco", 1, 0, 0,
           (SCM win),
           "Get the window decoration corresponding to the given window or icon deco.")
{
  Decoration* deco;
  must_be_valid_deco(s_get_deco_window, win, 1);
  deco = WL_DECO(win); 
  if (!deco->Win())
    return SCM_BOOL_F;
  return deco->Win()->Deco()->scm();
}

SCM_DEFINE(get_deco_inner, "inner-deco", 1, 0, 0,
           (SCM win),
           "Get the inner client (or menu) decoration corresponding to the given decoration.")
{
  Decoration* deco;
  must_be_valid_deco(s_get_deco_inner, win, 1);
  deco = WL_DECO(win); 
  if (!deco->Win())
    return SCM_BOOL_F;
  else
    return deco->Win()->Inner()->scm();
}

SCM_DEFINE(window_is_iconified, "window-iconified?", 1, 0, 0,
           (SCM window),
           "Check if window is iconified.")
{
  must_be_window(s_window_is_iconified, window, 1);
  return (WL_WINDOW(window)->Iconified() ? SCM_BOOL_T : SCM_BOOL_F);
}


SCM_DEFINE(IconifyWindow, "iconify-window", 1, 0, 0,
           (SCM window),
           "Iconify the window.")
{
  must_be_window(s_IconifyWindow, window, 1);
  WL_WINDOW(window)->Iconify();
  return SCM_UNSPECIFIED;
}

SCM_DEFINE(DeiconifyWindow, "deiconify-window", 1, 0, 0,
           (SCM window),
           "Deiconify the window.")
{
  must_be_window(s_DeiconifyWindow, window, 1);
  WL_WINDOW(window)->Deiconify();
  return SCM_UNSPECIFIED;
}

SCM_DEFINE(MoveWindow, "move-window", 3, 0, 0,
           (SCM window, SCM x, SCM y),
           "Move window to (x,y).")
{
  must_be_valid_deco(s_MoveWindow, window, 1);
  must_be_number(s_MoveWindow, x, 2);
  must_be_number(s_MoveWindow, y, 3);
  WL_DECO(window)->Top()->changepos(gh_scm2int(x), gh_scm2int(y));
  return SCM_UNSPECIFIED;
}

SCM_DEFINE(ResizeWindow, "resize-window", 3, 0, 0,
           (SCM window, SCM width, SCM height),
           "Resize window to width * height. Use the gravity hint to decide which sides"
           "should stay fixed. Only real client windows can be resized with this function.")
{
  int ww, hh, dw, dh;
  ClientWindow* cw;
  Decoration* deco;
  int ix1, ix2, iy1, iy2;

  must_be_valid_window(s_ResizeWindow, window, 1);
  must_be_number(s_ResizeWindow, width, 2);
  must_be_number(s_ResizeWindow, height, 3);
  cw = WL_WINDOW(window);
  deco = WL_DECO(window)->Top();
  if (!cw->Client() || cw->Client()->Top() != deco) return SCM_UNSPECIFIED;
  dw = gh_scm2int(width) - deco->Width() - 2 * deco->Borderwidth();
  dh = gh_scm2int(height) - deco->Height() - 2 * deco->Borderwidth();
  cw->Client()->GetInnerDims(ix1, iy1, ix2, iy2);
  ww = ix2-ix1+1 + dw;
  hh = iy2-iy1+1 + dh;
  conform_to_hints(cw->NormalHints(), &ww, &hh);
  cw->Client()->Resize(ww, hh);
  return SCM_UNSPECIFIED;
}

SCM_DEFINE(MoveResizeWindow, "move-resize-window", 5, 0, 0,
           (SCM window, SCM x, SCM y, SCM width, SCM height),
           "Move and resize window in one operation (bypassing gravity hint).")
{
  int ww, hh, dw, dh;
  ClientWindow* cw;
  Decoration* deco;
  int ix1, ix2, iy1, iy2, nx, ny;

  must_be_valid_window(s_MoveResizeWindow, window, 1);
  must_be_number(s_MoveResizeWindow, x, 2);
  must_be_number(s_MoveResizeWindow, y, 3);
  must_be_number(s_MoveResizeWindow, width, 4);
  must_be_number(s_MoveResizeWindow, height, 5);
  cw = WL_WINDOW(window);
  deco = WL_DECO(window)->Top();
  if (!cw->Client() || cw->Client()->Top() != deco) {
    deco->changepos(gh_scm2int(x), gh_scm2int(y));
    return SCM_UNSPECIFIED;
  }
  cw->Client()->GetInnerDims(ix1, iy1, ix2, iy2);
  nx = gh_scm2int(x) + ix1 - cw->Client()->Borderwidth();
  ny = gh_scm2int(y) + iy1 - cw->Client()->Borderwidth() ;
  dw = gh_scm2int(width) - deco->Width() - 2 * deco->Borderwidth();
  dh = gh_scm2int(height) - deco->Height() - 2 * deco->Borderwidth();
  ww = ix2-ix1+1 + dw;
  hh = iy2-iy1+1 + dh;
  conform_to_hints(cw->NormalHints(), &ww, &hh);
  cw->Client()->MoveResize(nx, ny, ww, hh);
  return SCM_UNSPECIFIED;
}

/*
 * Raise window (optionally % other top-level window)
 */

SCM_DEFINE(RaiseWindow, "raise-window", 1, 1, 0,
           (SCM window, SCM arg),
           "Raise window to top, or just above the optional second window if given.")
{
  XWindowChanges values;
  unsigned int nb_windows;
  Window *windows = 0, dummywin, parent;
  Window hook;
  Decoration* deco;
  Decoration* root;
  int i, j;
  must_be_valid_deco(s_RaiseWindow, window, 1);
  deco = WL_DECO(window)->Top();
  root = deco->Screen()->Deco();
  hook = deco->Xwin();
  if (root->check_issue_stack_event()) {
    XQueryTree(dpy, deco->Screen()->Root(), &dummywin, &parent,
               &windows, &nb_windows);
    for (i=0; i<(int)nb_windows && hook != windows[i]; i++);
  }
  if (arg == SCM_UNDEFINED) {
    XRaiseWindow(dpy, hook);
    if (windows) {
      for (j=i+1; j<(int)nb_windows && !LookUpDeco(windows[j]); j++);
      if (j < (int)nb_windows)  // Some decorated windows above
        root->issue_stack_event();
      XFree(windows);
    }
  } else {
    must_be_valid_deco(s_RaiseWindow, arg, 2);
    values.sibling = WL_DECO(arg)->Top()->Xwin();
    values.stack_mode = Above;
    if (values.sibling != hook) {
      XConfigureWindow(dpy, hook,
                       CWStackMode | CWSibling, &values);
      if (windows) {
        for (j=i-1; j>=0 && windows[j] != values.sibling && !LookUpDeco(windows[j]); j--);
        if (j < 0 || windows[j] != values.sibling)  // Not already above
          root->issue_stack_event();
        XFree(windows);
      }
    } else
      if (windows)
        XFree(windows);
  }
  return SCM_UNSPECIFIED;
}

/*
 * Lower window (optionally % other top-level window)
 */

SCM_DEFINE(LowerWindow, "lower-window", 1, 1, 0,
           (SCM window, SCM arg),
           "Lower window to bottom, or ust below the optional second window if given.")
{
  XWindowChanges values;
  unsigned int nb_windows;
  Window *windows = 0, dummywin, parent;
  Window hook;
  Decoration* deco;
  Decoration* root;
  int i, j;
  must_be_valid_deco(s_LowerWindow, window, 1);
  deco = WL_DECO(window)->Top();
  root = deco->Screen()->Deco();
  hook = deco->Xwin();
  if (root->check_issue_stack_event()) {
    XQueryTree(dpy, deco->Screen()->Root(), &dummywin, &parent,
               &windows, &nb_windows);
    for (i=0; i<(int)nb_windows && hook != windows[i]; i++);
  }
  if (arg == SCM_UNDEFINED) {
    XLowerWindow(dpy, hook);
    if (windows) {
      for (j=i-1; j>=0 && !LookUpDeco(windows[j]); j--);
      if (j >= 0)  // Some decorated windows below
        root->issue_stack_event();
      XFree(windows);
    }
  } else {
    must_be_valid_deco(s_LowerWindow, arg, 2);
    values.sibling = WL_DECO(arg)->Top()->Xwin();
    values.stack_mode = Below;
    if (values.sibling != hook) {
      XConfigureWindow(dpy, hook,
                       CWStackMode | CWSibling, &values);
      if (windows) {
        for (j=i+1; j<(int)nb_windows && windows[j] != values.sibling && !LookUpDeco(windows[j]); j++);
        if (j >= (int)nb_windows || windows[j] != values.sibling)  // Not already above
          root->issue_stack_event();
        XFree(windows);
      }
    } else
      if (windows)
        XFree(windows);
  }
  return SCM_UNSPECIFIED;
}

SCM_DEFINE(KillWindow, "kill-window", 1, 0, 0,
           (SCM window),
           "Forcibly destroy the window. May kill the entire client program, so try"
           "'delete-window' first.")
{
  ClientWindow* cw;
  must_be_window(s_KillWindow, window, 1);
  cw = WL_WINDOW(window);
  if (cw->Client())
    XKillClient(dpy, cw->InnerWin());
  else
    cw->UnDecorateWindow(0);
  return SCM_UNSPECIFIED;
}

SCM_DEFINE(ReDecorateWindow, "redecorate-window", 1, 0, 1,
           (SCM window, SCM args),
           "Reconstruct the decorations for the window and icon of the client. Possible key:"
           ":decoration      Alternative decoration function instead of 'describe-window'"
           ":icon-decoration Alternative icon decoration function instead of 'describe-icon'")
{
  ClientWindow* cw;
  ScreenContext* scr;
  Window win;
  SCM ctx, sub;
  int n, cn;
  SCM proc, iproc;
  XSetWindowAttributes wa;
  must_be_valid_window(s_ReDecorateWindow, window, 1);
  n = wl_separate_context(args, sub, ctx, cn, s_ReDecorateWindow);
  if (n != 0)
    gwm_wrong_num_args(s_ReDecorateWindow, n+1);
  proc = gwm_get_keyword(k_decoration, ctx, cn, SCM_UNDEFINED);
  iproc = gwm_get_keyword(k_icon_decoration, ctx, cn, SCM_UNDEFINED);
  cw = WL_WINDOW(window);
  scr = WL_SCREEN(window);
  if (proc != SCM_UNDEFINED) {
    if (!gh_procedure_p(proc))
      gwm_wrong_type_arg(s_ReDecorateWindow, 0, proc, "procedure");
  } else if (!cw->Client() && cw->Inner())
    proc = ((IMenu*) cw->Inner())->DecoProcedure();
  else
    proc = 0;
  if (iproc != SCM_UNDEFINED) {
    if (!gh_procedure_p(iproc))
      gwm_wrong_type_arg(s_ReDecorateWindow, 0, iproc, "procedure");
  } else if (!cw->Client() && cw->Inner())
    iproc = ((IMenu*) cw->Inner())->IconProcedure();
  else
    iproc = 0;
  if (!cw->IsClosing()) {
    win = cw->InnerWin();
    if (!cw->Client() && cw->Inner()) {
      wa.override_redirect = 0;
      XChangeWindowAttributes(dpy, win, CWOverrideRedirect, &wa);
    }
    cw->UnDecorateWindow(2); // deletes cw
    cw = DecorateWindow(win, scr, proc, 0);
    if (!cw)
      gwm_misc_error(s_ReDecorateWindow, "Failed to decorate window", 0);
    if (iproc)
      RealizeIconWindow(cw->Deco(), iproc);
    XSync(dpy, 0);
  }
  return cw->Deco()->scm();
}

/*
 * the "current-window" functions
 */

SCM_DEFINE(window_name, "window-name", 1, 0, 0,
           (SCM window),
           "Get the name of the client window.")
{
  must_be_window(s_window_name, window, 1);
  return WL_WINDOW(window)->WindowName();
}

SCM_DEFINE(window_name_set, "set-window-name!", 2, 0, 0,
           (SCM window, SCM name),
           "Set the name of the client window.")
{
  char* str;
  must_be_window(s_window_name_set, window, 1);
  str = wl_getstring(name, s_window_name_set, 2);
  if (WL_WINDOW(window)->Client() && !WL_WINDOW(window)->PendingClose()) {
    XStoreName(dpy, WL_WINDOW(window)->InnerWin(), str);
  }
  delete [] str;
  return SCM_UNSPECIFIED;
}

SCM_DEFINE(window_client_name, "window-client-name", 1, 0, 0,
           (SCM window),
           "Get the client name of the window.")
{
  must_be_window(s_window_client_name, window, 1);
  return WL_WINDOW(window)->ClientName();
}

SCM_DEFINE(window_client_class, "window-client-class", 1, 0, 0,
           (SCM window),
           "Get the client class of the window.")
{
  must_be_window(s_window_client_class, window, 1);
  return WL_WINDOW(window)->ClientClass();
}

SCM_DEFINE(window_machine_name, "window-machine-name", 1, 0, 0,
           (SCM window),
           "Get the name of the host on which the client runs.")
{
  must_be_window(s_window_machine_name, window, 1);
  return WL_WINDOW(window)->MachineName();
}

SCM_DEFINE(window_icon_name, "window-icon-name", 1, 0, 0,
           (SCM window),
           "Get the name of icon of the client window.")
{
  must_be_window(s_window_icon_name, window, 1);
  return WL_WINDOW(window)->IconName();
}

SCM_DEFINE(window_icon_name_set, "set-window-icon-name!", 2, 0, 0,
           (SCM window, SCM name),
           "Set the name of icon of the client window.")
{
  char* str;
  must_be_window(s_window_icon_name_set, window, 1);
  str = wl_getstring(name, s_window_icon_name_set, 2);
  if (WL_WINDOW(window)->Client() && !WL_WINDOW(window)->PendingClose()) {
    XSetIconName(dpy, WL_WINDOW(window)->InnerWin(), str);
  }
  delete [] str;
  return SCM_UNSPECIFIED;
}

SCM_DEFINE(window_client_x, "window-client-x", 1, 0, 0,
           (SCM window),
           "Get x position of the inner client window. Equivalent to '(deco-x (inner-deco window))'")
{
  int ix1, ix2, iy1, iy2;
  must_be_window(s_window_client_x, window, 1);
  WL_WINDOW(window)->Inner()->GetInnerDims(ix1, iy1, ix2, iy2);
  return gh_int2scm(ix1 + WL_WINDOW(window)->Deco()->Xpos());
}

SCM_DEFINE(window_client_y, "window-client-y", 1, 0, 0,
           (SCM window),
           "Get y position of the inner client window. Equivalent to '(deco-y (inner-deco window))'")
{
  int ix1, ix2, iy1, iy2;
  must_be_window(s_window_client_y, window, 1);
  WL_WINDOW(window)->Inner()->GetInnerDims(ix1, iy1, ix2, iy2);
  return gh_int2scm(iy1 + WL_WINDOW(window)->Deco()->Ypos());
}

SCM_DEFINE(window_client_width, "window-client-width", 1, 0, 0,
           (SCM window),
           "Get width of the inner client window. Equivalent to '(deco-width (inner-deco window))'")
{
  must_be_window(s_window_client_width, window, 1);
  return gh_int2scm(WL_WINDOW(window)->Inner()->Width());
}

SCM_DEFINE(window_client_height, "window-client-height", 1, 0, 0,
           (SCM window),
           "Get height of the inner client window. Equivalent to '(deco-height (inner-deco window))'")
{
  must_be_window(s_window_client_height, window, 1);
  return gh_int2scm(WL_WINDOW(window)->Inner()->Height());
}

SCM_DEFINE(window_client_borderwidth, "window-client-borderwidth", 1, 0, 0,
           (SCM window),
           "Get borderwidth of the inner client window. Equivalent to '(deco-borderwidth (inner-deco window))'")
{
  must_be_window(s_window_client_borderwidth, window, 1);
  return gh_int2scm(WL_WINDOW(window)->Inner()->Borderwidth());
}

SCM_DEFINE(window_client_is_shaped, "window-client-shaped?", 1, 0, 0,
           (SCM window),
           "Check if the inner client has a shaped window.")
{
  must_be_window(s_window_client_is_shaped, window, 1);
  return (WL_WINDOW(window)->Inner()->Shaped() ? SCM_BOOL_T : SCM_BOOL_F);
}

SCM_DEFINE(window_x, "window-x", 1, 0, 0,
           (SCM window),
           "Get x position of the top-most parent window. Equivalent to '(deco-x (top-deco window))'")
{
  must_be_valid_deco(s_window_x, window, 1);
  return gh_int2scm(WL_DECO(window)->Top()->Xpos());
}

SCM_DEFINE(window_y, "window-y", 1, 0, 0,
           (SCM window),
           "Get y position of the top-most parent window. Equivalent to '(deco-y (top-deco window))'")
{
  must_be_valid_deco(s_window_y, window, 1);
  return gh_int2scm(WL_DECO(window)->Top()->Ypos());
}

SCM_DEFINE(window_width, "window-width", 1, 0, 0,
           (SCM window),
           "Get width of the top-most parent window, including borderwidth")
{
  Decoration* d;
  must_be_valid_deco(s_window_width, window, 1);
  d = WL_DECO(window)->Top();
  return gh_int2scm(d->Width() + 2 * d->Borderwidth());
}

SCM_DEFINE(window_height, "window-height", 1, 0, 0,
           (SCM window),
           "Get height of the top-most parent window, including borderwidth")
{
  Decoration* d;
  must_be_valid_deco(s_window_height, window, 1);
  d = WL_DECO(window)->Top();
  return gh_int2scm(d->Height() + 2 * d->Borderwidth());
}

/* returns client-managed icon window, if any, but first check that it exists
 */

SCM_DEFINE(window_icon_window, "window-icon-window", 1, 0, 0,
           (SCM window),
           "Get the specific window to use for the icon, if any.")
{
  Decoration* deco;
  must_be_window(s_window_icon_window, window, 1);
  if ((deco = WL_WINDOW(window)->MakeIconWindow()))
    return deco->scm();
  else
    return SCM_BOOL_F;
}

/* each time this function is called, construct the icon pixmap from the hints
 * (this to allow for applications changing their icons and user to change
 * color via the foreground and background variables)
 */

SCM_DEFINE(window_icon_bitmap, "window-icon-pixmap", 1, 0, 1,
           (SCM window, SCM args),
           "Get the pixmap to use for the icon, if the client has specified any."
           "Uses the keys:"
           ":background"
           ":foreground")
{
  SCM pixmap;
  ClientWindow* cw;
  ScreenContext* scr;
  SCM sub, ctx;
  SCM fg, bg, sh;
  int n, cn;

  if ((n = wl_separate_context(args, sub, ctx, cn, s_window_icon_window)) != 0)
    gwm_wrong_num_args(s_window_icon_bitmap, n+1);
  must_be_window(s_window_icon_bitmap, window, 1);
  cw = WL_WINDOW(window);
  scr = WL_SCREEN(window);
  if (cw->WmHints()->flags & IconPixmapHint) {
    bg = gwm_get_keyword(k_background, ctx, cn, scr->pixel.White);
    if (!WLPAINTP(bg))
      gwm_wrong_type_arg(s_window_icon_bitmap, 0, bg, "color or pixmap");
    fg = gwm_get_keyword(k_foreground, ctx, cn, scr->pixel.Black);
    if (!WLPAINTP(fg))
      gwm_wrong_type_arg(s_window_icon_bitmap, 0, fg, "color or pixmap");
    sh = gwm_get_keyword(k_shape, ctx, cn, SCM_UNDEFINED);
    if (sh != SCM_UNDEFINED) {
      if (sh != WA_transparent && sh != WA_hole)
        gwm_wrong_type_arg(s_window_icon_bitmap, 0, sh, "'transparent or 'hole");
    }
    pixmap = gwm_pixmap_from_pixmap(cw->WmHints()->icon_pixmap, 
                                    (cw->WmHints()->flags & IconMaskHint ?
                                     cw->WmHints()->icon_mask : 0),
                                    fg, bg, (sh == WA_hole ? 1 :0));
    return pixmap;
  } else {
    return SCM_BOOL_F;
  }
}

SCM_DEFINE(window_icon_bitmap_id, "window-icon-pixmap-id", 1, 0, 0,
           (SCM window),
           "Get the X id for the client specified icon bitmap. May be used to"
           "keep track of changes.")
{
  must_be_window(s_window_icon_bitmap_id, window, 1);
  if (WL_WINDOW(window)->WmHints()->flags & IconPixmapHint) {
    return gh_int2scm(WL_WINDOW(window)->WmHints()->icon_pixmap);
  } else {
    return SCM_BOOL_F;
  }
}

SCM_DEFINE(window_US_position, "window-user-set-position?", 1, 0, 0,
           (SCM window),
           "Return true if the user specified the position of the client window"
           "(via e.g. a -geometry argument).")
{
  ClientWindow* cw;
  must_be_window(s_window_US_position, window, 1);
  cw = WL_WINDOW(window);
  if (WL_DECO(window)->Type() & IconStatus) {
    if (cw->WmHints()->flags & IconPositionHint)
      return SCM_BOOL_T;
  } else {
    if (cw->NormalHints()->flags & USPosition)
      return SCM_BOOL_T;
  }
  return SCM_BOOL_F;
}

SCM_DEFINE(window_US_size, "window-user-set-size?", 1, 0, 0,
           (SCM window),
           "Return true if the user specified the size of the client window"
           "(via e.g. a -geometry argument).")
{
  must_be_window(s_window_US_size, window, 1);
  if (WL_WINDOW(window)->NormalHints()->flags & USSize)
    return SCM_BOOL_T;
  else
    return SCM_BOOL_F;
}

SCM_DEFINE(window_P_position, "window-program-set-position?", 1, 0, 0,
           (SCM window),
           "Return true if the program has specified a position of the client window.")
{
  must_be_window(s_window_P_position, window, 1);
  if (WL_WINDOW(window)->NormalHints()->flags & PPosition)
    return SCM_BOOL_T;
  else
    return SCM_BOOL_F;
}

SCM_DEFINE(window_P_size, "window-program-set-size?", 1, 0, 0,
           (SCM window),
           "Return true if the program has specified a size of the client window.")
{
  must_be_window(s_window_P_size, window, 1);
  if (WL_WINDOW(window)->NormalHints()->flags & PSize)
    return SCM_BOOL_T;
  else
    return SCM_BOOL_F;
}

SCM_DEFINE(window_is_transient_for, "window-transient-for", 1, 0, 0,
           (SCM window),
           "Check if window is a transient window, and for which window.")
{
  Window tf;
  must_be_window(s_window_is_transient_for, window, 1);
  tf = WL_WINDOW(window)->TransientFor();
  if (tf) {
    return SemiDecoratedWindow(tf);
  } else {
    return SCM_BOOL_F;
  }
}

int gwm_decode_gravity(SCM val)
{
  if (val == WA_northwest)
    return 1;
  else if (val == WA_north)
    return 2;
  else if (val == WA_northeast)
    return 3;
  else if (val == WA_west)
    return 4;
  else if (val == WA_center)
    return 5;
  else if (val == WA_east)
    return 6;
  else if (val == WA_southwest)
    return 7;
  else if (val == WA_south)
    return 8;
  else if (val == WA_southeast)
    return 9;
  else if (val == WA_static)
    return StaticGravity;
  else if (val == WA_forget)
    return ForgetGravity;
  else
    return -1;
}

SCM_DEFINE(window_gravity, "window-gravity", 1, 0, 0,
           (SCM window),
           "Get the window gravity hint.")
{
  int grav;
  must_be_window(s_window_gravity, window, 1);
  if (WL_WINDOW(window)->NormalHints()->flags & PWinGravity)
    grav = WL_WINDOW(window)->NormalHints()->win_gravity;
  else
    grav = 1;
  switch (grav) {
  case 1: return WA_northwest;
  case 2: return WA_north;
  case 3: return WA_northeast;
  case 4: return WA_west;
  case 5: return WA_center;
  case 6: return WA_east;
  case 7: return WA_southwest;
  case 8: return WA_south;
  case 9: return WA_southeast;
  case StaticGravity: return WA_static;
  default: return WA_forget;
  }
}

SCM_DEFINE(window_gravity_set, "set-window-gravity!", 2, 0, 0,
           (SCM window, SCM val),
           "Set the window gravity hint.")
{
  int grav;
  must_be_window(s_window_gravity_set, window, 1);
  grav = gwm_decode_gravity(val);
  if (grav < 0)
    gwm_wrong_type_arg(s_window_gravity_set, 2, val, "gravity symbol");
  WL_WINDOW(window)->NormalHints()->win_gravity = grav;
  WL_WINDOW(window)->NormalHints()->flags |= PWinGravity;
  return SCM_UNSPECIFIED;
}

/* declare accepted icons sizes for future clients
 */

SCM_DEFINE(set_wm_icon_sizes, "set-icon-sizes!", 6, 0, 0,
           (SCM minwidth, SCM minheight, SCM maxwidth, SCM maxheight, SCM incwidth, SCM incheight),
           "Set icon size preferences.")
{
  XIconSize	icon_sizes;
  must_be_number(s_set_wm_icon_sizes, minwidth, 1);
  must_be_number(s_set_wm_icon_sizes, minheight, 2);
  must_be_number(s_set_wm_icon_sizes, maxwidth, 3);
  must_be_number(s_set_wm_icon_sizes, maxheight, 4);
  must_be_number(s_set_wm_icon_sizes, incwidth, 5);
  must_be_number(s_set_wm_icon_sizes, incheight, 6);
  icon_sizes.min_width = gh_scm2int(minwidth);
  icon_sizes.min_height = gh_scm2int(minheight);
  icon_sizes.max_width = gh_scm2int(maxwidth);
  icon_sizes.max_height = gh_scm2int(maxheight);
  icon_sizes.width_inc = gh_scm2int(incwidth);
  icon_sizes.height_inc = gh_scm2int(incheight);
  XSetIconSizes(dpy, Context->Root(), &icon_sizes, 6);
  return SCM_UNSPECIFIED;
}

/* get WM_COMMAND
 */

SCM_DEFINE(get_wm_command, "get-wm-command", 1, 0, 0,
           (SCM arg),
           "Get the restart command from a client window.")
{
  ClientWindow* cw;
  Atom actual_type;
  int actual_format = 0;
  unsigned long nitems = 0L, leftover = 0L, i;
  unsigned char *prop = NULL;
  SCM list;
  int nstrings = 0, n;
  unsigned char *p;
  must_be_window(s_get_wm_command, arg, 1);
  cw = WL_WINDOW(arg);
  if (!cw->SaveYourselfMessageOk() || cw->PendingClose())
    return SCM_BOOL_F;
  cw->send_protocol_message(XA_WM_SAVE_YOURSELF, 0, 0);
  XSync(dpy, 0);
  if (XGetWindowProperty (dpy, cw->InnerWin(), XA_WM_COMMAND,
                          0L, 1000000L, False,
                          AnyPropertyType, &actual_type, &actual_format,
                          &nitems, &leftover, &prop) == Success &&
      actual_type != None && prop && nitems) {
    for (i = 0; i < nitems; i++)
      if (prop[i] == '\0')
      	nstrings ++;
    list = SCM_EOL;
    p = prop;
    for (n = 0; n < nstrings; n++) {
      list = gh_cons(scm_makfrom0str((char*) p), list);
      while(*p++);
    }
    list = scm_reverse_x(list, SCM_EOL);
    XFree(prop);
    return list;
  } else
    return SCM_BOOL_F;
}

/* WM_DELETE_WINDOW
 * if window participate in protocol, sends message and returns TRUE
 * else unmaps it and its associated icon (go to withdrawn) and returns FALSE
 */

SCM_DEFINE(delete_window, "delete-window", 1, 0, 0,
           (SCM arg),
           "Ask the client window to delete itself. Returns true if it succeeded.")
{
  must_be_window(s_delete_window, arg, 1);
  if (WL_WINDOW(arg)->DeleteMessageOk()) {
    if (!WL_WINDOW(arg)->PendingClose())
      WL_WINDOW(arg)->send_protocol_message(XA_WM_DELETE_WINDOW, 0, 0);
    return SCM_BOOL_T;
  } else
    return SCM_BOOL_F;
}

/*
 * set focus on
 * 	current client if no arg (do nothing if input hint is false)
 * 	other window's client if arg
 * 	if arg = #t, focus follows pointer
 */

SCM_DEFINE(set_focus, "set-focus!", 1, 0, 0,
           (SCM deco),
           "Set focus to deco, or to pointer-root if argument is #t. To set focus to"
           "a client window, call 'set-focus' on the inner-deco of the window.")
{
  ClientWindow* cw;
  Decoration* d;
  if (deco == SCM_BOOL_T) {
    XSetInputFocus(dpy, PointerRoot, RevertToPointerRoot, CurrentTime);
    return SCM_BOOL_T;
  } else {
    must_be_valid_deco(s_set_focus, deco, 1);
    d = WL_DECO(deco);
    cw = d->Win();
  }
  if (GWM_No_set_focus)
    return SCM_BOOL_F;
  if (cw && cw->Client() && d == cw->Inner()) {
    if (cw->PendingClose())
      return SCM_BOOL_F;
    if (cw->TakeFocusMessageOk())
      cw->send_protocol_message(XA_WM_TAKE_FOCUS, 0, 0);
    if (!(cw->WmHints()->flags & InputHint) ||   // some clients are buggy, does this work ?
        (cw->WmHints()->input))
      TrapXErrors(XSetInputFocus(dpy, cw->InnerWin(),
                                 RevertToPointerRoot, CurrentTime));
  } else {
    TrapXErrors(XSetInputFocus(dpy, d->MaybeMenu()->Xwin(),
                               RevertToPointerRoot, CurrentTime));
  }
  return SCM_BOOL_T;
}

SCM_DEFINE(get_focus, "get-focus", 0, 0, 0,
           (),
           "Return the deco which has the focus, or #t if pointer-root, or #f if"
           "no or an unknown window has the focus.")
{
  Decoration* wob;
  Window win;
  int rev;
  XGetInputFocus(dpy, &win, &rev);
  if (win == None)
    return SCM_BOOL_F;
  if (win == PointerRoot)
    return SCM_BOOL_T;
  if ((wob = LookUpDeco(win)))
    return wob->scm();
  else if ((wob = LookUpInnerClient(win)))
    return wob->scm();
  else
    return SCM_BOOL_F;
}

SCM_DEFINE(window_is_focusable, "window-focusable?", 1, 0, 0,
           (SCM window),
           "Check if the client window can take focus.")
{
  ClientWindow* cw;
  must_be_window(s_window_is_focusable, window, 1);
  cw = WL_WINDOW(window);
  if (cw->Client())
    return (cw->TakeFocusMessageOk() ||
            !(cw->WmHints()->flags & InputHint) ||
            (cw->WmHints()->input) ?
            SCM_BOOL_T : SCM_BOOL_F);
  else
    return (cw->Inner()->MaybeMenu()->GetFsm()->CanUseFocus() ?
            SCM_BOOL_T : SCM_BOOL_F);
}

/* Colormaps
 * colormap focus management:
 */
SCM_DEFINE(set_colormap_focus, "set-colormap-focus!", 1, 0, 0,
           (SCM window),
           "Set the used colormap to that of the given window or screen.")
{
  if (WLSCREENP(window)) {
    WL_SCREEN(window)->SetDefaultColormap();
  } else if (WLWINDOWP(window)) {
    WL_WINDOW(window)->SetColormapFocus();
  } else
    gwm_wrong_type_arg(s_set_colormap_focus, 1, window, "window or screen");
  return SCM_UNSPECIFIED;
}

/* Sub windows colormaps
 * colormap focus management:
 */
SCM_DEFINE(set_subwindow_colormap_focus, "set-subwindow-colormap-focus!", 1, 1, 0,
           (SCM window, SCM arg),
           "Change the colormap focus between the subwindows of window. An optional"
           "second argument is the index of the subwindow to use.")
{
  int index;
  must_be_window(s_set_subwindow_colormap_focus, window, 1);
  if (arg != SCM_UNDEFINED) {
    must_be_number(s_set_subwindow_colormap_focus, arg, 2);
    index = gh_scm2int(arg);
  } else
    index = -1;
  WL_WINDOW(window)->SetColormapFocusIndex(index);
  return SCM_UNSPECIFIED;
}

/*
 * Window groups
 */
SCM_DEFINE(window_group, "window-group", 1, 0, 0,
           (SCM window),
           "Get the window group of the window, or an empty list if there are no group.")
{
  ClientWindow* cw;
  GroupHead* head;

  must_be_window(s_window_group, window, 1);
  cw = WL_WINDOW(window);
  if ((head = cw->WindowGroupHead())) { /* group exists */
    return head->group;
  } else {				/* no group */
    return SCM_EOL;
  }
}

SCM_DEFINE(window_group_set, "set-window-group!", 2, 0, 0,
           (SCM window, SCM gl),
           "Set the window group of the window, by either specifying the group leader,"
           "or a list of the entire group. An empty list removes the window from any group.")
{
  ClientWindow* cw;

  must_be_window(s_window_group_set, window, 1);
  cw = WL_WINDOW(window);
  if (gl == SCM_EOL) {
    cw->RemoveWindowFromGroup();
  } else if (WLWINDOWP(gl)) {
    cw->AddWindowToGroupLeader(WL_WINDOW(gl)->InnerWin());
  } else if (gh_list_p(gl) && WLWINDOWP(SCM_CAR(gl))) {
    cw->AddWindowToGroupLeader(WL_WINDOW(SCM_CAR(gl))->InnerWin());
  } else if (gh_list_p(gl) && scm_is_integer(SCM_CAR(gl))) {
    cw->AddWindowToGroupLeader(gh_scm2int(SCM_CAR(gl)));
  } else
    gwm_wrong_type_arg(s_window_group_set, 2, gl, "window or list of windows");
  return SCM_UNSPECIFIED;
}

SCM_DEFINE(window_min_size, "window-min-size", 1, 0, 0,
           (SCM window),
           "Return a list of the minimum width and height in pixels according"
           "to the windows size hints.")
{
  XSizeHints* hints;
  must_be_window(s_window_min_size, window, 1);
  hints = WL_WINDOW(window)->NormalHints();
  if (hints->flags & PMinSize) {
    return gh_list(gh_int2scm(hints->min_width), gh_int2scm(hints->min_height), SCM_UNDEFINED);
  } else {
    return gh_list(gh_int2scm(0), gh_int2scm(0), SCM_UNDEFINED);
  }
}

SCM_DEFINE(window_max_size, "window-max-size", 1, 0, 0,
           (SCM window),
           "Return a list of the maximum width and height in pixels according"
           "to the windows size hints. Unintuitively, (-1 -1) means unlimited.")
{
  XSizeHints* hints;
  must_be_window(s_window_max_size, window, 1);
  hints = WL_WINDOW(window)->NormalHints();
  if (hints->flags & PMaxSize) {
    return gh_list(gh_int2scm(hints->max_width), gh_int2scm(hints->max_height), SCM_UNDEFINED);
  } else {
    return gh_list(gh_int2scm(-1), gh_int2scm(-1), SCM_UNDEFINED);
  }
}

SCM_DEFINE(conform_window_size, "conform-window-size", 3, 0, 0,
           (SCM window, SCM width, SCM height),
           "Return a list of an acceptable width and height in pixels according"
           "to the windows size hints, as close as possible to (but not larger than)"
           "the given width and height.")
{
  int ww, hh, dw, dh;
  int ix1, ix2, iy1, iy2;
  ClientWindow* cw;
  must_be_valid_window(s_conform_window_size, window, 1);
  must_be_number(s_conform_window_size, width, 2);
  must_be_number(s_conform_window_size, height, 3);
  cw = WL_WINDOW(window);
  if (cw->Client()) {
    cw->Client()->GetInnerDims(ix1, iy1, ix2, iy2);
    dw = cw->Deco()->Width() + 2 * cw->Deco()->Borderwidth() - ix2 + ix1 - 1;
    dh = cw->Deco()->Height() + 2 * cw->Deco()->Borderwidth() - iy2 + iy1 - 1;
    ww = gh_scm2int(width) - dw;
    hh = gh_scm2int(height) - dh;
    conform_to_hints(cw->NormalHints(), &ww, &hh);
    return gh_list(gh_int2scm(ww + dw), gh_int2scm(hh + dh), SCM_UNDEFINED);
  } else { // Gwm menus can not be resized
    ww = cw->Deco()->Width() + 2 * cw->Deco()->Borderwidth();
    hh = cw->Deco()->Height() + 2 * cw->Deco()->Borderwidth();
    return gh_list(gh_int2scm(ww), gh_int2scm(hh), SCM_UNDEFINED);
  }
}

/*
 * logical size of a window is size divided by resize increments (+ base)
 */
void pixel_to_logical_size(XSizeHints* hints, unsigned int width, unsigned int height, unsigned int* rw, unsigned int* rh)
{
    *rw = width;
    *rh = height;
    if (hints->flags & PResizeInc) {
	*rw = ((width) - hints->base_width) / hints->width_inc;
	*rh = ((height) - hints->base_height) / hints->height_inc;
    }
}

void logical_to_pixel_size(XSizeHints* hints, unsigned int width, unsigned int height, unsigned int* rw, unsigned int* rh)
{
    *rw = width;
    *rh = height;
    if (hints->flags & PResizeInc) {
	*rw = hints->base_width + width * hints->width_inc;
	*rh = hints->base_height + height * hints->height_inc;
    }
}

SCM_DEFINE(window_logical_size, "window-size", 1, 0, 0,
           (SCM window),
           "Return a list of the width and height of window in logical units.")
{
  SCM wl_list = SCM_EOL;
  unsigned int w = 0, h = 0;
  ClientWindow* cw;
  int ix1, ix2, iy1, iy2;
  must_be_window(s_window_logical_size, window, 1);
  cw = WL_WINDOW(window);
  if (cw->Client()) {
    cw->Client()->GetInnerDims(ix1, iy1, ix2, iy2);
    pixel_to_logical_size(cw->NormalHints(), ix2-ix1+1, iy2-iy1+1, &w, &h); 
    wl_list = gh_list(gh_int2scm(w), gh_int2scm(h), SCM_UNDEFINED);
  }
  return wl_list;
}

SCM_DEFINE(window_logical_size_set, "set-window-size!", 2, 0, 0,
           (SCM window, SCM list),
           "Resize window to the width and height in list given in logical units.")
{
  unsigned int w = 0, h = 0;
  ClientWindow* cw;
  must_be_valid_window(s_window_logical_size_set, window, 1);
  cw = WL_WINDOW(window);
  if (!gh_list_p(list) || scm_ilength(list) != 2 ||
      !scm_is_integer(SCM_CAR(list)) || !scm_is_integer(SCM_CAR(SCM_CDR(list))))
    gwm_wrong_type_arg(s_window_logical_size_set, 2, list, "list of two numbers");
  if (cw->Client()) {
    logical_to_pixel_size(cw->NormalHints(),
                          gh_scm2int(SCM_CAR(list)),
                          gh_scm2int(SCM_CAR(SCM_CDR(list))),
                          &w, &h);
    cw->Client()->Resize(w, h);
  }
  return SCM_UNSPECIFIED;
}

void init_scm_wops()
{
#include "wops.x"
}
