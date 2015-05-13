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


 /*********************\
 * 		       *
 *  Fsm Object         *
 * 		       *
 \*********************/

#include <stdio.h>

#include <guile/gh.h>
#include <libguile/macros.h>

#include "gwm.hh"
#include "gwmfunc.hh"
#include "error.hh"
#include "cursor.hh"
#include "deco.hh"
#include "screen.hh"
#include "client.hh"
#include "event.hh"
#include "fsm.hh"

/*
extern long scm_tc16_behavior;
extern long scm_tc16_eventarc;

class EventArc {
public:
  EventArc(SCM ev, SCM act, SCM st);
  virtual SCM Action() { return (action == SCM_UNDEFINED ? SCM_BOOL_F : action); };
  virtual SCM State() { return (state == SCM_UNDEFINED ? SCM_BOOL_F : state); };
  virtual SCM StealResend() { return SCM_BOOL_F; };
  virtual SCM Resend() { return SCM_BOOL_F; };
  virtual int NoGrab() { return 1; };
  virtual void Mark();
  SCM event;
  SCM action;
  SCM state;
  SCM self;
};

class KeyButtonEventArc : public EventArc {
public:
  KeyButtonEventArc(SCM ev, SCM act, SCM st, SCM stl, SCM res) : EventArc(ev, act, st) { steal = stl; resend = res; };
  virtual SCM StealResend() { return (steal == SCM_UNDEFINED ? (resend == SCM_UNDEFINED ? SCM_BOOL_F : scm_cons(SCM_BOOL_T, resend)) : (resend == SCM_UNDEFINED ? scm_cons(steal, SCM_BOOL_F) : scm_cons(steal, resend))); };
  virtual SCM Resend() { return (resend == SCM_UNDEFINED ? SCM_BOOL_F : resend); };
  virtual int NoGrab() { return ((steal == SCM_UNDEFINED || steal == SCM_BOOL_F) && resend == SCM_UNDEFINED); };
  virtual void Mark();
  SCM steal;
  SCM resend;
};

class PressReleaseEventArc : public KeyButtonEventArc {
public:
  PressReleaseEventArc(SCM ev, SCM act, SCM act2, SCM st, SCM stl, SCM res) : KeyButtonEventArc(ev, act, st, stl, res) { action2 = act2; };
  virtual SCM Action() { return (action == SCM_UNDEFINED && action2 == SCM_UNDEFINED ? SCM_BOOL_F : scm_cons((action == SCM_UNDEFINED ? SCM_BOOL_F : action), (action2 == SCM_UNDEFINED ? SCM_BOOL_F : action2))); };
  virtual void Mark();
  SCM action2;
};

struct ArcLink {
  ArcLink(EventArc* a, ArcLink* n) { arc = a; next = n; };
  EventArc* arc;
  ArcLink* next;
};

struct FsmLink {
  FsmLink(class Fsm* f, FsmLink* n) { fsm = f; next = n; };
  class Fsm* fsm;
  FsmLink* next;
};

struct FsmArcLink {
  FsmArcLink(class Fsm* f, EventArc* a, SCM e, class Behavior* o, FsmArcLink* n) { fsm = f; arc = a; ev = e; oldstate = o; next = n; if (ev) scm_gc_protect_object(ev); };
  ~FsmArcLink() { if (ev) scm_gc_unprotect_object(ev); if (next) delete next; };
  class Fsm* fsm;
  EventArc* arc;
  SCM ev;
  class Behavior* oldstate;
  FsmArcLink* next;
};

class Behavior {
public:
    Behavior();
    ~Behavior();
    EventArc* locatearc(struct EventPat* pat, ArcLink*& prev, int& ind);
    unsigned int checkgrabs();
    void addarc(SCM obj);
    void addbehavior(SCM obj);
    EventArc* get_arc(XEvent* evt, int onlygrab, int mb);
    void install(Fsm* fsm);
    void uninstall(Fsm* fsm);
    void updatefsms();
    void establish_grabs(Window win, int etype);
    SCM scm() { return self; };
    unsigned int mask;
    unsigned int gwmmask;
    unsigned int grabmask;
    SCM self;
    ArcLink** arctable;
    FsmLink* conts;
};

class Fsm {
    friend class ClientFsm;
public:
    Fsm(class Decoration* w, Fsm* par, unsigned int emask);
    virtual ~Fsm();
    virtual void change_parent(Fsm* par);
    virtual void install_state(Behavior* beh);
    virtual void install_nullstate();
    virtual void reinstall_state();
    virtual void updatemask(unsigned int m, unsigned int gm, unsigned int pm, int change);
    virtual int action(XEvent* evt);
    virtual void simple_action(XEvent* evt, EventArc* arc, int half);
    virtual int check_redir_action(FsmArcLink*& retarcs, FsmArcLink*& retfirst, XEvent* evt, unsigned int m, int first, int mb);
    virtual void redir_action(FsmArcLink* arcs, XEvent* evt, int first, int resent, int half);
    void refresh_second_action(FsmArcLink* arcs, XEvent* evt);
    int check_second_action(FsmArcLink*& ret, FsmArcLink* farcs, XEvent* evt, int fcont, unsigned int m, int first);
    void second_action(FsmArcLink* arcs);
    int check_stolen_release(XEvent* evt);
    int check_simple_maxmulti(XEvent* evt, int& rsflag);
    int check_redir_maxmulti(XEvent* evt, int& rsflag);
    void SetGrab(class Decoration* gpar, unsigned int smask, int key, int cld, int cnf, int nfr, int asc, class WlCursor* cur);
    void RemoveGrab(int asc);
    unsigned int GwmMask() { return gwmmask; };
    int CanUseFocus() { return (mask & (KeyPressMask | KeyReleaseMask | FocusChangeMask)); };
    static void ClearGrabs();
    static int ServerGrabbed();
    static void TempFreezeOn();
    static void TempFreezeOff();
    static void register_enter(XCrossingEvent* ev, class Decoration* to);
    static Decoration* FindGrab(class Decoration* w, int gk);
    static Decoration* GetGrabTarget(class Decoration* wob, XEvent* ev);
protected:
    int IsAncestor(Fsm* par);
    static void UpdateGrab();
    static struct GrabEle* LocateGrab(class Decoration* w, struct GrabEle*& e1, struct GrabEle*& e2, int asc);
    static struct GrabEle* GrabStack;
    unsigned int mask;
    unsigned int gwmmask;
    unsigned int grabmask;
    unsigned int parentmask;
    unsigned int extramask;
    Behavior* state;
    class Decoration* wob;
    Fsm* parent;
    FsmLink* sons;
    static class Decoration* entered_deco;
};

class ClientFsm : public Fsm {
public:
    ClientFsm(class Decoration* w, Fsm* par, unsigned int emask);
    virtual void updatemask(unsigned int m, unsigned int gm, unsigned int pm, int change);
    virtual int action(XEvent* evt);
protected:
    unsigned int oldgrabmask;
};

#define WLBEHAVIORP(x) (SCM_NIMP(x) && SCM_CAR(x) == (SCM)scm_tc16_behavior)
#define WLEVENTARCP(x) (SCM_NIMP(x) && SCM_CAR(x) == (SCM)scm_tc16_eventarc)
#define WL_BEHAVIOR(x) ((Behavior*) SCM_CDR(x))
#define WL_EVENTARC(x) ((EventArc*) SCM_CDR(x))

*/

long scm_tc16_behavior;
long scm_tc16_eventarc;

SCM mark_eventarc(SCM obj)
{
  WL_EVENTARC(obj)->Mark();
  return SCM_BOOL_F;
}

void EventArc::Mark()
{
  if (event) scm_gc_mark(event);
  if (action) scm_gc_mark(action);
  if (state) scm_gc_mark(state);
}

void KeyButtonEventArc::Mark()
{
  EventArc::Mark();
  if (steal) scm_gc_mark(steal);
  if (resend) scm_gc_mark(resend);
}

void PressReleaseEventArc::Mark()
{
  KeyButtonEventArc::Mark();
  if (action2) scm_gc_mark(action2);
}

size_t free_eventarc(SCM obj)
{
  delete WL_EVENTARC(obj);
  return 0;
};

int print_eventarc(SCM obj, SCM port, scm_print_state * pstate)
{
  scm_puts("#<eventarc>", port);
  return 1;
};


SCM eventarc2scm(EventArc* arc)
{
  return scm_cell((scm_t_bits) scm_tc16_eventarc, (scm_t_bits) arc);
}

SCM_DEFINE(wl_eventarc_p, "eventarc?", 1, 0, 0,
           (SCM obj),
           "Return true if obj is a behavior event arc.")
{
  return (WLEVENTARCP(obj) ? SCM_BOOL_T : SCM_BOOL_F);
}

/*
 * make an arc: (FSUBR)
 * syntax: (on event body...)
 */

SCM_SYNTAX(s_m_make_event_arc, "on", scm_makmmacro, (SCM_FUNC_CAST_ARBITRARY_ARGS) m_make_event_arc);

SCM m_make_event_arc (SCM xorig, SCM env)
{
  SCM ctx, sub;
  int cn;
  SCM args = SCM_CDR (xorig);
  wl_separate_context(args, sub, ctx, cn, s_m_make_event_arc);
  return scm_cons(WA_on_event,
           scm_cons(SCM_CAR(sub),
             scm_cons(scm_cons(scm_sym_lambda,
                        scm_cons(scm_listify(WA_deco, WA_event, SCM_UNDEFINED),
                                 SCM_CDR(sub))),
               ctx)));
}

SCM_DEFINE(make_event_arc, "on-event", 1, 0, 1,
           (SCM ev, SCM args),
           "(on-event event-descriptor thunk [key val] ...)"
           "(on-event key-or-button-event-descriptor thunk1 [thunk2] [key val] ...)"
           "Make an event arc for a behavior. When an event matching event-descriptor"
           "occurs, the thunk is run. The second form is for keys, buttons and"
           "multibuttons, which may take one thunk to run at press and one to run at"
           "release. Keywords are:"
           ":behavior        New behavior for the state machine."
           ":steal           Steal corresponding events from parts."
           ":resend          Steal events from parts and resend them when done."
           "The last two keywords can only be given for events that are meaningful"
           "to steal from inner decorations or clients, i.e. buttons or keys and"
           "their presses and releases.")
{
  int n, cn, i;
  SCM ctx, sub;
  EventArc* arc;
  SCM action, action2, steal, resend, state;
  int bk = 0, pr = 0;
  if (!WLEVENTPATP(ev))
    gwm_wrong_type_arg(s_make_event_arc , 1, ev, "event");
  if ((i = WL_EVENTPAT(ev)->Index()) < 7) bk = 1;
  if (i == 1 || i == 2 || i == 5) pr = 1;
  n = wl_separate_context(args, sub, ctx, cn, s_make_event_arc);
  if (pr ? n > 2 : n > 1)
    gwm_wrong_num_args(s_make_event_arc, 1+n);
  if (n == 0) {
    action = action2 = SCM_UNDEFINED;
  } else {
    if (!gh_procedure_p(SCM_CAR(sub)))
      gwm_wrong_type_arg(s_make_event_arc , 2, SCM_CAR(sub), "procedure");
    action = SCM_CAR(sub);
    if (n == 2) {
      if (!gh_procedure_p(SCM_CAR(SCM_CDR(sub))))
        gwm_wrong_type_arg(s_make_event_arc , 3, SCM_CAR(SCM_CDR(sub)), "procedure");
      action2 = SCM_CAR(SCM_CDR(sub));
    } else
      action2 = SCM_UNDEFINED;
  }
  state = gwm_get_keyword(k_behavior, ctx, cn, SCM_UNDEFINED);
  if (state != SCM_UNDEFINED && !WLBEHAVIORP(state) && !gh_procedure_p(state))
    gwm_wrong_type_arg(s_make_event_arc , 0, state, "behavior or procedure");
  steal = gwm_get_keyword(k_steal, ctx, cn, SCM_UNDEFINED);
  resend = gwm_get_keyword(k_resend, ctx, cn, SCM_UNDEFINED);
  if (bk) {
    if (pr)
      arc = new PressReleaseEventArc(ev, action, action2, state, steal, resend);
    else
      arc = new KeyButtonEventArc(ev, action, state, steal, resend);
  } else {
    if (steal != SCM_UNDEFINED)
      gwm_warning("Stealing not applicable for event.", 0);
    if (resend != SCM_UNDEFINED)
      gwm_warning("Resending not applicable for event.", 0);
    arc = new EventArc(ev, action, state);
  }
  return arc->self;
}

SCM_DEFINE(eventarc_event, "eventarc-event", 1, 0, 0,
           (SCM arc),
           "Retreive the event descriptor for the event arc.")
{
  if (!WLEVENTARCP(arc))
    gwm_wrong_type_arg(s_eventarc_event , 1, arc, "eventarc");
  return WL_EVENTARC(arc)->event;
}

SCM_DEFINE(eventarc_action, "eventarc-action", 1, 0, 0,
           (SCM arc),
           "Retreive the action thunk of the event arc, or a pair of thunks in the"
           "case of buttons or keys.")
{
  if (!WLEVENTARCP(arc))
    gwm_wrong_type_arg(s_eventarc_action , 1, arc, "eventarc");
  return WL_EVENTARC(arc)->Action();
}

SCM_DEFINE(eventarc_behavior, "eventarc-behavior", 1, 0, 0,
           (SCM arc),
           "Retrieve the new behavior state of the event arc.")
{
  if (!WLEVENTARCP(arc))
    gwm_wrong_type_arg(s_eventarc_behavior , 1, arc, "eventarc");
  return WL_EVENTARC(arc)->State();
}

SCM_DEFINE(eventarc_steal_resend, "eventarc-steal-resend", 1, 0, 0,
           (SCM arc),
           "Retrieve the values of :steal and :resend as a pair, or false if they"
           "are not given or the event arc is of the wrong type.")
{
  if (!WLEVENTARCP(arc))
    gwm_wrong_type_arg(s_eventarc_steal_resend, 1, arc, "eventarc");
  return WL_EVENTARC(arc)->StealResend();
}

EventArc::EventArc(SCM ev, SCM act, SCM st)
{
  event = ev;
  action = act;
  state = st;
  self = eventarc2scm(this);
}



SCM mark_behavior(SCM obj)
{
  int i;
  ArcLink* p;
  for (i=0; i<GWMNumEvents; i++) {
    p = WL_BEHAVIOR(obj)->arctable[i];
    while (p) {
      scm_gc_mark(p->arc->self);
      p = p->next;
    }
  }
  return SCM_BOOL_F;
}

size_t free_behavior(SCM obj)
{
  delete WL_BEHAVIOR(obj);
  return 0;
};

int print_behavior(SCM obj, SCM port, scm_print_state * pstate)
{
  scm_puts("#<behavior>", port);
  return 1;
};


SCM behavior2scm(Behavior* beh)
{
  return scm_cell((scm_t_bits) scm_tc16_behavior, (scm_t_bits) beh);
}

SCM_DEFINE(wl_behavior_p, "behavior?", 1, 0, 0,
           (SCM obj),
           "Return true if obj is a behavior.")
{
  return (WLBEHAVIORP(obj) ? SCM_BOOL_T : SCM_BOOL_F);
}


/*
 * Routine used to construct a state 
 * USAGE:
 * 	(make-behavior arc1 arc2 ... arcn)
 */

SCM_DEFINE(make_behavior, "make-behavior", 0, 0, 1,
           (SCM args),
           "Make a new behavior out of the supplied arcs and behaviors.")
{
  Behavior* state;
  int i;
  SCM arg;
  
  for (i = 0, arg=args; SCM_NNULLP(arg); i++,arg=SCM_CDR(arg))
    if (!WLBEHAVIORP(SCM_CAR(arg)) && !WLEVENTARCP(SCM_CAR(arg)))
      gwm_wrong_type_arg(s_make_behavior, i+1, SCM_CAR(arg), "eventarc or behavior");
  state = new Behavior();
  scm_gc_protect_object(state->scm()); // prevent GC during creation
  for (i = 0, arg=scm_reverse_x(args, SCM_EOL); SCM_NNULLP(arg); i++,arg=SCM_CDR(arg)) {
    if (WLEVENTARCP(SCM_CAR(arg)))
      state->addarc(SCM_CAR(arg));
    else if (WLBEHAVIORP(SCM_CAR(arg)))
      state->addbehavior(SCM_CAR(arg));
  }
  scm_gc_unprotect_object(state->scm());
  return state->scm();
}

SCM_DEFINE(modify_behavior, "modify-behavior", 1, 0, 1,
           (SCM beh, SCM args),
           "Change the arcs in the behavior according to the supplied arcs and behaviors.")
{
  Behavior* state;
  int i;
  SCM arg;
  
  if (!WLBEHAVIORP(beh))
    gwm_wrong_type_arg(s_modify_behavior, 1, beh, "behavior");
  for (i = 0, arg=args; SCM_NNULLP(arg); i++,arg=SCM_CDR(arg))
    if (!WLBEHAVIORP(SCM_CAR(arg)) && !WLEVENTARCP(SCM_CAR(arg)))
      gwm_wrong_type_arg(s_modify_behavior, i+2, SCM_CAR(arg), "eventarc or behavior");
  state = WL_BEHAVIOR(beh);
  for (i = 0, arg=scm_reverse_x(args, SCM_EOL); SCM_NNULLP(arg); i++,arg=SCM_CDR(arg)) {
    if (WLEVENTARCP(SCM_CAR(arg)))
      state->addarc(SCM_CAR(arg));
    else if (WLBEHAVIORP(SCM_CAR(arg)))
      state->addbehavior(SCM_CAR(arg));
  }
  state->updatefsms();
  return SCM_UNSPECIFIED;
}

SCM_DEFINE(inspect_behavior, "inspect-behavior", 1, 1, 0,
           (SCM beh, SCM arg),
           "Without the optional second argument, gives a list of the behaviors all"
           "event arcs. With the extra event descriptor argument, returns the corresponding"
           "event arc if any.")
{
  Behavior* state;
  EventArc* arc;
  ArcLink* tmp;
  int i, ind;
  SCM res;
  if (!WLBEHAVIORP(beh))
    gwm_wrong_type_arg(s_inspect_behavior, 1, beh, "behavior");
  state = WL_BEHAVIOR(beh);
  if (arg != SCM_UNDEFINED) {
    if (!WLEVENTPATP(arg))
      gwm_wrong_type_arg(s_inspect_behavior, 2, arg, "event");
    arc = state->locatearc(WL_EVENTPAT(arg), tmp, ind);
    if (arc)
      return arc->self;
    else
      return SCM_BOOL_F;
  } else {
    res = SCM_EOL;
    for (i=0; i<GWMNumEvents; i++)
      for (tmp=state->arctable[i]; tmp; tmp=tmp->next)
        res = scm_cons(scm_cons(tmp->arc->event, tmp->arc->self), res);
    return scm_reverse_x(res, SCM_EOL);
  }
}

Behavior::Behavior()
{
  int i;
  mask = 0;
  gwmmask = 0;
  grabmask = 0;
  arctable = new ArcLink*[GWMNumEvents];
  for (i=0; i<GWMNumEvents; i++)
    arctable[i] = 0;
  conts = 0;
  self = behavior2scm(this);
}

Behavior::~Behavior()
{
  int i;
  ArcLink *a1, *a2;
  for (i=0; i<GWMNumEvents; i++) {
    a1 = arctable[i];
    while (a1) {
      a2 = a1;
      a1 = a1->next;
      delete a2;
    }
  }
  delete [] arctable;
}

EventArc* Behavior::locatearc(EventPat* pat, ArcLink*& prev, int& ind)
{
  ArcLink *link, *tmp1 = 0, *tmp2 = 0;
  int ok = 0;
  ind = pat->Index();
  link = arctable[ind];
  switch (ind) {
  case 0: 
  case 2: 
  case 3:
    while (link && ((EventPatButton*) pat)->lt(WL_EVENTPATBUTTON(link->arc->event)))
      tmp1 = link, link = link->next;
    tmp2 = tmp1;
    while (link &&
           !(ok = ((EventPatButton*) pat)->eq(WL_EVENTPATBUTTON(link->arc->event))) &&
           !WL_EVENTPATBUTTON(link->arc->event)->lt((EventPatButton*) pat))
      tmp1 = link, link = link->next;
    break;
  case 1:
    while (link && ((EventPatMultibutton*) pat)->lt(WL_EVENTPATMULTIBUTTON(link->arc->event)))
      tmp1 = link, link = link->next;
    tmp2 = tmp1;
    while (link &&
           !(ok = ((EventPatMultibutton*) pat)->eq(WL_EVENTPATMULTIBUTTON(link->arc->event))) &&
           !WL_EVENTPATMULTIBUTTON(link->arc->event)->lt((EventPatMultibutton*) pat))
      tmp1 = link, link = link->next;
    break;
  case 4:
  case 5:
  case 6:
    while (link && ((EventPatKey*) pat)->lt(WL_EVENTPATKEY(link->arc->event)))
      tmp1 = link, link = link->next;
    tmp2 = tmp1;
    while (link &&
           !(ok = ((EventPatKey*) pat)->eq(WL_EVENTPATKEY(link->arc->event))) &&
           !WL_EVENTPATKEY(link->arc->event)->lt((EventPatKey*) pat))
      tmp1 = link, link = link->next;
    break;
  case 17:
    while (link &&
           !(ok = ((EventPatUser*) pat)->eq(WL_EVENTPATUSER(link->arc->event))))
      tmp1 = link, link = link->next;
    break;
  case 18:
    while (link &&
           !(ok = ((EventPatPropertychange*) pat)->eq(WL_EVENTPATPROPERTYCHANGE(link->arc->event))))
      tmp1 = link, link = link->next;
    break;
  case 19:
    while (link &&
           !(ok = ((EventPatClientmessage*) pat)->eq(WL_EVENTPATCLIENTMESSAGE(link->arc->event))))
      tmp1 = link, link = link->next;
    break;
  default: 
    ok = (link ? 1 : 0);
  }
  if (ok) {
    prev = tmp1;
    return link->arc;
  } else {
    prev = tmp2;
    return 0;
  }
}

unsigned int Behavior::checkgrabs()
{
  int i;
  ArcLink *p;
  unsigned int res = 0;
  for (i=0; i<7; i++) {
    p = arctable[i];
    while (p && p->arc->NoGrab())
      p = p->next;
    if (p)
      res |= WL_EVENTPAT(p->arc->event)->Mask();
  }
  return res;
}

void Behavior::addarc(SCM obj)
{
  unsigned int m;
  int ind;
  EventArc* a;
  ArcLink *p, *p2;
  EventArc* arc = WL_EVENTARC(obj);
  m = WL_EVENTPAT(arc->event)->Mask();
  a = locatearc(WL_EVENTPAT(arc->event), p, ind);
  if (arc->action != SCM_UNDEFINED) {
    mask |= m;
    gwmmask |= WL_EVENTPAT(arc->event)->GwmMask();
    if (a) {
      if (p)
        p->next->arc = arc;
      else
        arctable[ind]->arc = arc;
    } else {
      if (p)
        p->next = new ArcLink(arc, p->next);
      else
        arctable[ind] = new ArcLink(arc, arctable[ind]);
    }
  } else if (a) {
    // remove mask bits ? ***
    if (p) {
      p2 = p->next;
      p->next = p->next->next;
      delete p2;
    } else {
      p2 = arctable[ind];
      arctable[ind] = arctable[ind]->next;
      delete p2;
    }
  }
  if (!arc->NoGrab() || grabmask & m)
    grabmask = checkgrabs();
}

void Behavior::addbehavior(SCM obj)
{
  int i, ind;
  ArcLink *p, *p1;
  EventArc* a;
  Behavior* beh = WL_BEHAVIOR(obj);
  mask |= beh->mask;
  gwmmask |= beh->gwmmask;
  for (i=0; i<GWMNumEvents; i++) {
    p1 = beh->arctable[i];
    while (p1) {
      a = locatearc(WL_EVENTPAT(p1->arc->event), p, ind);
      if (a) {
        if (p)
          p->next->arc = p1->arc;
        else
          arctable[ind]->arc = p1->arc;
      } else {
        if (p)
          p->next = new ArcLink(p1->arc, p->next);
        else
          arctable[ind] = new ArcLink(p1->arc, arctable[ind]);
      }
      p1 = p1->next;
    }
  }
  if (beh->grabmask || grabmask)
    grabmask = checkgrabs();
}

void Behavior::updatefsms()
{
  FsmLink *f1;
  f1 = conts;
  while (f1 && f1->fsm) {
    f1->fsm->reinstall_state();
    f1 = f1->next;
  }
}

EventArc* Behavior::get_arc(XEvent* evt, int onlygrab, int mb)
{
  int i, rel = 255;
  ArcLink* p;
  if (evt->type == ButtonPress) {
    if (scm_is_integer(SCM_VARIABLE_REF(v_relevant_modifiers)))
      rel = gh_scm2int(SCM_VARIABLE_REF(v_relevant_modifiers));
    p = arctable[0];
    while (p && ((onlygrab && p->arc->NoGrab()) ||
                 !(WL_EVENTPATBUTTON(p->arc->event)->match2(evt, rel))))
      p = p->next;
    if (!p) {
      if (mb > 1) {
        p = arctable[1];
        while (p && ((onlygrab && p->arc->NoGrab()) ||
                     !(WL_EVENTPATMULTIBUTTON(p->arc->event)->match3(evt, rel, mb))))
          p = p->next;
      } else {
        p = arctable[2];
        while (p && ((onlygrab && p->arc->NoGrab()) ||
                     !(WL_EVENTPATBUTTON(p->arc->event)->match2(evt, rel))))
          p = p->next;
      }
    } 
  } else if (evt->type == KeyPress) {
    if (scm_is_integer(SCM_VARIABLE_REF(v_relevant_modifiers)))
      rel = gh_scm2int(SCM_VARIABLE_REF(v_relevant_modifiers));
    for (i=4; i<=5; i++) {
      p = arctable[i];
      while (p && ((onlygrab && p->arc->NoGrab()) ||
                   !(WL_EVENTPATKEY(p->arc->event)->match2(evt, rel))))
        p = p->next;
      if (p) break;
    }
  } else {
    i = fetch_event_index(evt);
    if (i == -1) return 0;
    p = arctable[i];
    if (i == 3 || i == 6) { // Buttonrelease or KeyRelease
      if (scm_is_integer(SCM_VARIABLE_REF(v_relevant_modifiers)))
        rel = gh_scm2int(SCM_VARIABLE_REF(v_relevant_modifiers));
      while (p && ((onlygrab && p->arc->NoGrab()) ||
                   !(i == 3 ? WL_EVENTPATBUTTON(p->arc->event)->match2(evt, rel) : WL_EVENTPATKEY(p->arc->event)->match2(evt, rel))))
        p = p->next;
    } else {
      while (p && ((onlygrab && p->arc->NoGrab()) ||
                   !(WL_EVENTPAT(p->arc->event)->match(evt))))
        p = p->next;
    }
  }
  return (p ? p->arc : 0);
}

void Behavior::install(Fsm* fsm)
{
  conts = new FsmLink(fsm, conts);
}

void Behavior::uninstall(Fsm* fsm)
{
  FsmLink *f1, *f2;
  f1 = conts;
  f2 = 0;
  while (f1 && f1->fsm != fsm) {
    f2 = f1;
    f1 = f1->next;
  }
  if (f1) {
    if (f2)
      f2->next = f1->next;
    else
      conts = f1->next;
    delete f1;
  }
}

Decoration* Fsm::entered_deco = 0;

Fsm::Fsm(Decoration* w, Fsm* par, unsigned int emask)
{
  mask = UINT_MAX;
  gwmmask = 0;
  grabmask = 0;
  extramask = emask;
  parentmask = (par ? par->parentmask | par->grabmask : 0);
  state = 0;
  wob = w;
  parent = par;
  sons = 0;
  if (par)
    par->sons = new FsmLink(this, par->sons);
}

ClientFsm::ClientFsm(Decoration* w, Fsm* par, unsigned int emask)
  : Fsm(w, par, emask)
{
  oldgrabmask = 0;
  if (parentmask)
    updatemask(mask, grabmask, parentmask, 1);
}

Fsm::~Fsm()
{
  FsmLink *f1, *f2;
  if (state)
    state->uninstall(this);
  if (parent) {
    f1 = parent->sons;
    f2 = 0;
    while (f1 && f1->fsm != this)
      f2 = f1, f1 = f1->next;
    if (f1) {
      if (f2)
        f2->next = f1->next;
      else
        parent->sons = f1->next;
      delete f1;
    }
  }
  f1 = sons;
  while (f1) {
    f2 = f1;
    f1 = f1->next;
    if (f2->fsm)
      f2->fsm->parent = 0;
    delete f2;
  }
}

void Fsm::change_parent(Fsm* par)
{
  FsmLink *f1, *f2;
  if (parent) {
    f1 = parent->sons;
    f2 = 0;
    while (f1 && f1->fsm != this)
      f2 = f1, f1 = f1->next;
    if (f1) {
      if (f2)
        f2->next = f1->next;
      else
        parent->sons = f1->next;
      delete f1;
    }
  }
  parent = par;
  if (par) {
    updatemask(mask, grabmask, par->parentmask | par->grabmask, parentmask || par->parentmask || par->grabmask);
    par->sons = new FsmLink(this, par->sons);
  } else
    updatemask(mask, grabmask, 0, parentmask);
}


void Fsm::install_state(Behavior* beh)
{
  if (state)
    state->uninstall(this);
  state = beh;
  state->install(this);
  gwmmask = beh->gwmmask;
  updatemask(beh->mask, beh->grabmask, parentmask, grabmask || beh->grabmask);
}

void Fsm::install_nullstate()
{
  if (state)
    state->uninstall(this);
  state = 0;
  gwmmask = 0;
  updatemask(0, 0, parentmask, grabmask);
}

void Fsm::reinstall_state()
{
  if (!state)
    return;
  gwmmask = state->gwmmask;
  updatemask(state->mask, state->grabmask, parentmask, grabmask || state->grabmask);
}

void Fsm::updatemask(unsigned int m, unsigned int gm, unsigned int pm, int change)
{
  FsmLink* f;
  unsigned int key = (KeyPressMask | KeyReleaseMask);
  if (m != mask || (parentmask & key) != (pm & key)) {
    mask = m;
    parentmask = pm;
    XSelectInput(dpy, wob->Xwin(), mask | extramask | (parentmask & key));
  }
  if (change) {
    grabmask = gm; 
    parentmask = pm;
    f = sons;
    while (f) {
      f->fsm->updatemask(f->fsm->mask, f->fsm->grabmask, parentmask | grabmask, 1);
      f = f->next;
    }
  }
}
  
void ClientFsm::updatemask(unsigned int m, unsigned int gm, unsigned int pm, int change)
{
  int but, key, tmask;
  Fsm* f;
  Window win;
  tmask = gm | pm | parentmask | grabmask | oldgrabmask;
  but = tmask & (ButtonPressMask | ButtonReleaseMask);
  key = tmask & (KeyPressMask | KeyReleaseMask);
  if ((key || but) && !wob->Win()->PendingClose()) {
    win = wob->Win()->InnerWin();
    if ((parentmask | oldgrabmask) & but)
      XUngrabButton(dpy, AnyButton, AnyModifier, win);
    if ((parentmask | oldgrabmask) & key)
      XUngrabKey(dpy, AnyKey, AnyModifier, win);
    f = this;
    Fsm::updatemask(m, gm, pm, change);
    oldgrabmask = grabmask;
    while (f && ((f->parentmask | f->grabmask) & (but | key))) {
      if (f->state && (f->grabmask & but)) {
        f->state->establish_grabs(win, 0);
        f->state->establish_grabs(win, 1);
        f->state->establish_grabs(win, 2);
        f->state->establish_grabs(win, 3);
      }
      if (f->state && (f->grabmask & key)) {
        f->state->establish_grabs(win, 4);
        f->state->establish_grabs(win, 5);
        f->state->establish_grabs(win, 6);
      }
      f = f->parent;
    }
  } else {
    Fsm::updatemask(m, gm, pm, change);
    oldgrabmask = grabmask;
  }
}

void Behavior::establish_grabs(Window win, int etype)
{
  ArcLink* p;
  p = arctable[etype];
  while (p) {
    if (!p->arc->NoGrab())
      WL_EVENTPAT(p->arc->event)->set_grab(win);
    p = p->next;
  }
}

/* the main routine:
 * check the incoming event against all transitions of the current
 * state of the fsm, and trigger the action if necessary
 */

int Fsm::action(XEvent* evt)
{
  FsmArcLink* arcs;
  FsmArcLink* firstarc;
  EventArc* arc;
  unsigned int m;
  int res, cont, mb = 0, half = 1;
  static int tmask = (ButtonPressMask | ButtonReleaseMask | KeyPressMask | KeyReleaseMask);
  m = fetch_event_mask(evt);
  if (!(tmask & m)) {
    if (!state || (m ? !(mask & m) : !(gwmmask & fetch_event_gwmmask(evt))))
      return 0;
    arc = state->get_arc(evt, 0, mb);
    if (!arc) return 0;
    simple_action(evt, arc, half);
    return 1;
  } else {
    if (evt->type == ButtonRelease || evt->type == KeyRelease)
      if (RegisterRelease(evt, cont)) {
        return 0;
      }
    if (evt->type == ButtonPress) {
      mb = check_redir_maxmulti(evt, res);
      res = 0;
      mb = count_multibutton(evt, mb, res, half);
    }
    cont = check_redir_action(arcs, firstarc, evt, m, 1, mb);
    if (firstarc) {
      firstarc->next = arcs;
      arcs = firstarc;
      cont = 0;
    }
    if (arcs) {
      arcs->fsm->redir_action(arcs, evt, 1, 0, half);
      return 1;
    } else
      return 0;
  }
}

int AnyPressRelease(FsmArcLink* arcs)
{
  int i;
  while (arcs) {
    if ((i = WL_EVENTPAT(arcs->arc->event)->Index()) == 1 || i == 2 || i == 5)
      return 1;
    arcs = arcs->next;
  }
  return 0;
}

int ClientFsm::action(XEvent* evt)
{
  static EventArc* arcbut = 0;
  static EventArc* arckey = 0;
  FsmArcLink* arcs;
  FsmArcLink* firstarc;
  unsigned int m;
  int res = 0, mb = 0, half = 1, cont;
  static int tmask = (ButtonPressMask | ButtonReleaseMask | KeyPressMask | KeyReleaseMask);
  m = fetch_event_mask(evt);
  if (evt->xany.window != wob->Win()->InnerWin() || !(m & tmask))
    return Fsm::action(evt);
  else {
    GWM_processing_frozen = 1;
    if (evt->type == ButtonRelease || evt->type == KeyRelease)
      if (RegisterRelease(evt, cont)) {
        XAllowEvents(dpy, (evt->type == ButtonRelease ? 
                           (cont ? ReplayPointer : AsyncPointer) :
                           (cont ? ReplayKeyboard : AsyncKeyboard)), CurrentTime);
        GWM_processing_frozen = 0;
        return 0;
      }
    if (evt->type == ButtonPress) {
      mb = check_redir_maxmulti(evt, res);
      mb = count_multibutton(evt, mb, res, half);
    }
    cont = check_redir_action(arcs, firstarc, evt, m, 1, mb); 
    if ((evt->type == ButtonPress && res == 0) || evt->type == ButtonRelease) {
      if (GWM_processing_frozen)
        if (cont) {
          XAllowEvents(dpy, ReplayPointer, CurrentTime);
          if (check_bounced_event(evt)) {
            if (firstarc)
              firstarc->next = arcs, arcs = firstarc;
            cont = 0;
          } else if (firstarc)
            delete firstarc;
        } else
          XAllowEvents(dpy, AsyncPointer, CurrentTime);
    } else if (evt->type == KeyPress || evt->type == KeyRelease) {
      if (GWM_processing_frozen)
        if (cont) {
          XAllowEvents(dpy, ReplayKeyboard, CurrentTime);
          if (check_bounced_event(evt)) {
            if (firstarc)
              firstarc->next = arcs, arcs = firstarc;
            cont = 0;
          } else if (firstarc)
            delete firstarc;
        } else
          XAllowEvents(dpy, AsyncKeyboard, CurrentTime);
    } else if (evt->type == ButtonPress && cont) {
      if (res == RESENT_SYNT)
        resend_multibutton(evt, mb, !half, res);
      if (res == RESENT_BOUNCED && firstarc)
        firstarc->next = arcs, arcs = firstarc, cont = 0;
      else if (firstarc)
        delete firstarc;
    }
    GWM_processing_frozen = 0;
    if (cont && (evt->type == ButtonPress || evt->type == KeyPress) && !AnyPressRelease(arcs))
      if (check_stolen_release(evt) || res == RESENT_SYNT) {
        if (evt->type == ButtonPress) {
          if (!arcbut) {
            arcbut = new PressReleaseEventArc(eventpat2scm(new EventPatButton(-1, -1)),
                                              SCM_UNDEFINED, SCM_UNDEFINED,
                                              SCM_UNDEFINED, SCM_UNDEFINED,
                                              SCM_UNDEFINED);
            scm_gc_protect_object(arcbut->self);
          }
          arcs = new FsmArcLink(this, arcbut, 0, 0, arcs);
        } else {
          if (!arckey) {
            arckey = new PressReleaseEventArc(eventpat2scm(new EventPatKey(-1, -1)),
                                              SCM_UNDEFINED, SCM_UNDEFINED,
                                              SCM_UNDEFINED, SCM_UNDEFINED,
                                              SCM_UNDEFINED);
            scm_gc_protect_object(arckey->self);
          }
          arcs = new FsmArcLink(this, arckey, 0, 0, arcs);
        }
      }
    if (arcs) {
      arcs->fsm->redir_action(arcs, evt, 1, (cont ? (res == RESENT_SYNT ? RESENT_SYNT : RESENT_NORMAL) : 0), half);
      return 1;
    }
    return 0;
  }
}

int Fsm::check_stolen_release(XEvent*evt)
{
  Fsm* fsm = this;
  ArcLink* p;
  int rel = 255;
  int ind = (evt->type == ButtonPress ? 3 : 6);
  if (scm_is_integer(SCM_VARIABLE_REF(v_relevant_modifiers)))
    rel = gh_scm2int(SCM_VARIABLE_REF(v_relevant_modifiers));
  while (fsm) {
    if (fsm->state) {
      p = fsm->state->arctable[ind];
      while (p && (p->arc->NoGrab() ||
                   !(ind == 3 ? WL_EVENTPATBUTTON(p->arc->event)->match2(evt, rel) : WL_EVENTPATKEY(p->arc->event)->match2(evt, rel))))
        p = p->next;
      if (p)
        return 1;
    }
    fsm = fsm->parent;
  }
  return 0;
}

int Fsm::check_simple_maxmulti(XEvent* evt, int& rsflag)
{
  ArcLink* p;
  int mnum, rnum, rel = 255;
  if (!state) {
    rsflag = 0;
    return 0;
  }
  if (scm_is_integer(SCM_VARIABLE_REF(v_relevant_modifiers)))
    rel = gh_scm2int(SCM_VARIABLE_REF(v_relevant_modifiers));
  p = state->arctable[0];
  while (p && !(WL_EVENTPATBUTTON(p->arc->event)->match2(evt, rel)))
    p = p->next;
  if (p) {
    rsflag = 0;
    return 0;
  }
  p = state->arctable[1];
  while (p && !(WL_EVENTPATBUTTON(p->arc->event)->match2(evt, rel)))
    p = p->next;
  if (p) {
    rnum = mnum = WL_EVENTPATMULTIBUTTON(p->arc->event)->Num();
    rsflag = (p->arc->Resend() == SCM_BOOL_T ? RESENT_NORMAL : RESENT_SYNT);
    while (p && rsflag == RESENT_NORMAL) {
      while (p && (!(WL_EVENTPATBUTTON(p->arc->event)->match2(evt, rel)) ||
                   mnum == WL_EVENTPATMULTIBUTTON(p->arc->event)->Num()))
        p = p->next;
      if (p)
        mnum = WL_EVENTPATMULTIBUTTON(p->arc->event)->Num();
      if (p && p->arc->Resend() != SCM_BOOL_T)
        rsflag = RESENT_SYNT;
    }
    if (p && rsflag == RESENT_NORMAL) {
      p = state->arctable[2];
      while (p && !(WL_EVENTPATBUTTON(p->arc->event)->match2(evt, rel)))
        p = p->next;
      if (p && p->arc->Resend() != SCM_BOOL_T)
        rsflag = RESENT_SYNT;
    }
    return rnum;
  }
  p = state->arctable[2];
  while (p && !(WL_EVENTPATBUTTON(p->arc->event)->match2(evt, rel)))
    p = p->next;
  if (p) {
    rsflag = 0;
    return 1;
  } else {
    rsflag = 0;
    return 0;
  }
}

int Fsm::check_redir_maxmulti(XEvent* evt, int& rsflag)
{
  int maxm = 0;
  int ret1 = RESENT_NORMAL, ret2 = RESENT_NORMAL;
  if ((grabmask | mask) & ButtonPressMask)
    maxm = check_simple_maxmulti(evt, ret1);
  if (parent)
    maxm = Max(maxm, parent->check_redir_maxmulti(evt, ret2));
  if (maxm > 1)
    rsflag = (ret1 == RESENT_NORMAL && ret2 == RESENT_NORMAL ? RESENT_NORMAL : RESENT_SYNT);
  else
    rsflag = 0;
  return maxm;
}

void Fsm::simple_action(XEvent* evt, EventArc* arc, int half)
{
  SCM newstate;
  Behavior* oldstate;
  EventPat* ep;
  int i;
  SCM ev = event2scm(new WlEvent(evt, wob));
  if (arc->state != SCM_UNDEFINED) {
    if (WLBEHAVIORP(arc->state)) {
      wob->set_behavior(arc->state);
    } else if (gh_procedure_p(arc->state)) {
      oldstate = state;
      newstate = gwm_apply2_catch(arc->state, wob->scm(), ev);
      if (WLBEHAVIORP(newstate) && state == oldstate)
        wob->set_behavior(newstate);
    }
  }
  ep = WL_EVENTPAT(arc->event);
  if (arc->action != SCM_UNDEFINED)
    gwm_apply2_catch(arc->action, wob->scm(), ev);
}

int Fsm::check_redir_action(FsmArcLink*& retarcs, FsmArcLink*& retfirst, XEvent* evt, unsigned int m, int first, int mb)
{
  FsmArcLink* link = 0;
  FsmArcLink* dummy = 0;
  KeyButtonEventArc* arc = 0;
  Decoration* d;
  Behavior* oldstate;
  Fsm* f;
  int sflag, rflag;
  int cont;
  SCM ev;

  if (parentmask & m) {
    if (Fsm::ServerGrabbed() && parent && parent->wob != Fsm::GetGrabTarget(parent->wob, evt))
      cont = 1;
    else
      cont = parent->check_redir_action(link, dummy, evt, m, 0, mb);
  } else
    cont = 1;
  if (!cont) {
    retarcs = link;
    retfirst = 0;
    return 0;
  }
  if ((grabmask & m) &&
      (arc = (KeyButtonEventArc*) state->get_arc(evt, 1, mb))) {
    ev = event2scm(new WlEvent(evt, wob));
    oldstate = state;
    if (arc->steal == SCM_BOOL_F ||
               (gh_procedure_p(arc->steal) &&
                gwm_apply2_catch(arc->steal, wob->scm(), ev) == SCM_BOOL_F)) {
      sflag = 0;
      rflag = 1;
    } else if (arc->resend != SCM_UNDEFINED &&
               arc->resend != SCM_BOOL_F &&
               !(gh_procedure_p(arc->resend) &&
                 gwm_apply2_catch(arc->resend, wob->scm(), ev) == SCM_BOOL_F)) {
      sflag = 1;
      rflag = 1;
    } else {
      sflag = 1;
      rflag = 0;
    }
    if (sflag) {
      retarcs = new FsmArcLink(this, arc, ev, oldstate, link);
      retfirst = 0;
      return rflag;
    } else if (!first) {
      retarcs = link;
      retfirst = 0;
      return rflag;
    }
  }
  if (first) {
    f = this;
    while (f && (!link || f != link->fsm) &&
           (!(f->mask & m) || !(arc = (KeyButtonEventArc*) f->state->get_arc(evt, 0, mb))))
      f = f->parent;
    if (arc) {
      ev = event2scm(new WlEvent(evt, f->wob));
      retarcs = link;
      retfirst = new FsmArcLink(f, arc, ev, f->state, 0);
      return 1;
    }
  }
  retarcs = link;
  retfirst = 0;
  return cont;
}

void Fsm::redir_action(FsmArcLink* arcs, XEvent* evt, int first, int resent, int half)
{
  int dofirst = 0;
  EventArc* arc = arcs->arc;
  SCM ev = arcs->ev;
  SCM newstate;
  if (first && AnyPressRelease(arcs))
    dofirst = 1;
  if (arc->state != SCM_UNDEFINED) {
    if (WLBEHAVIORP(arc->state)) {
      if (arcs->fsm->state == arcs->oldstate)
        wob->set_behavior(arc->state);
    } else if (gh_procedure_p(arc->state)) {
      newstate = gwm_apply2_catch(arc->state, wob->scm(), ev);
      if (WLBEHAVIORP(newstate) && arcs->fsm->state == arcs->oldstate)
        wob->set_behavior(newstate);
    }
  }
  if (dofirst)
    SetupReleaseGuard(evt, wob, arcs, resent, half);
  if (arcs->next)
    arcs->next->fsm->redir_action(arcs->next, evt, 0, resent, half);
  if (arc->action != SCM_UNDEFINED)
    gwm_apply2_catch(arc->action, wob->scm(), ev);
  if (dofirst)
    CatchNormalRelease(evt);
  else if (first && arcs)
    delete arcs;
}

int Fsm::IsAncestor(Fsm* par)
{
  Fsm* f = this;
  while (f) {
    if(f == par)
      return TRUE;
    f = f->parent;
  }
  return FALSE;    
}

void Fsm::refresh_second_action(FsmArcLink* arcs, XEvent* evt)
{
  int i;
  EventArc* arc = arcs->arc;
  if ((((i = WL_EVENTPAT(arc->event)->Index()) == 1 || i == 2 || i == 5) &&
       ((PressReleaseEventArc*)arc)->action2 != SCM_UNDEFINED) ||
      ((i == 3 || i == 6) && arc->action != SCM_UNDEFINED)) {
    if (arcs->ev)
      scm_gc_unprotect_object(arcs->ev);
    arcs->ev = event2scm(new WlEvent(evt, wob));
    scm_gc_protect_object(arcs->ev);
  }
  if (arcs->next)
    arcs->next->fsm->refresh_second_action(arcs->next, evt);
}

int Fsm::check_second_action(FsmArcLink*& ret, FsmArcLink* farcs, XEvent* evt, int fcont, unsigned int m, int first)
{
  FsmArcLink *link = 0;
  FsmArcLink *farcs1, *farcs2 = 0;
  KeyButtonEventArc* arc = 0;
  Behavior* oldstate;
  Fsm* f;
  SCM ev;
  int sflag, rflag;
  int cont, i;
  if (first) {
    farcs1 = farcs;
    while (farcs && !IsAncestor(farcs->fsm))
      farcs2 = farcs, farcs = farcs->next;
    if (farcs2) {
      farcs2->next = 0;
      farcs1->fsm->refresh_second_action(farcs1, evt);
    }
  }
  if (parentmask & m)
    cont = parent->check_second_action(link, (farcs && farcs->fsm == this ? farcs->next : farcs),
                                       evt, 1, m, 0);
  else {
    cont = 1;
    link = (farcs && farcs->fsm == this ? farcs->next : farcs);
    if (link)
      link->fsm->refresh_second_action(link, evt);
  }
  if (farcs && farcs->fsm == this) farcs->next = 0;
  if (!cont) {
    if (farcs2)
      farcs2->next = link, ret = farcs1;
    else
      ret = link;
    if (farcs && farcs->fsm == this) delete farcs;
    return 0;
  }
  if (farcs && farcs->fsm == this) {
    if (farcs->arc &&
        !(farcs->ev == 0 && farcs->arc->action == SCM_UNDEFINED) &&
        ((i = WL_EVENTPAT(farcs->arc->event)->Index()) == 1 || i == 2 || i == 5)) {
      farcs->fsm->refresh_second_action(farcs, evt);
      farcs->next = link;
      if (farcs2)
        farcs2->next = farcs, ret = farcs1;
      else
        ret = farcs;
      return fcont;
    } else {
      delete farcs;
    }
  }
  if ((grabmask & m) &&
      (arc = (KeyButtonEventArc*) state->get_arc(evt, 1, 0))) {
    ev = event2scm(new WlEvent(evt, wob));
    oldstate = state;
    if (arc->steal == SCM_BOOL_F ||
               (gh_procedure_p(arc->steal) &&
                gwm_apply2_catch(arc->steal, wob->scm(), ev) == SCM_BOOL_F)) {
      sflag = 0;
      rflag = 1;
    } else if (arc->resend != SCM_UNDEFINED &&
               arc->resend != SCM_BOOL_F &&
               !(gh_procedure_p(arc->resend) &&
                 gwm_apply2_catch(arc->resend, wob->scm(), ev) == SCM_BOOL_F)) {
      sflag = 1;
      rflag = 1;
    } else {
      sflag = 1;
      rflag = 0;
    }
    if (sflag) {
      ret = new FsmArcLink(this, arc, ev, oldstate, link);
      if (farcs2)
        farcs2->next = ret, ret = farcs1;
      return rflag;
    } else if (!(first && !fcont)) {
      ret = link;
      if (farcs2)
        farcs2->next = ret, ret = farcs1;
      return rflag;
    }
  }
  if (first && !fcont) {
    f = this;
    while (f && (!link || f != link->fsm) &&
           (!(f->mask & m) || !(arc = (KeyButtonEventArc*) f->state->get_arc(evt, 0, 0))))
      f = f->parent;
    if (arc) {
      ev = event2scm(new WlEvent(evt, f->wob));
      link = new FsmArcLink(f, arc, ev, f->state, link);
      cont = 0;
    }
  }
  if (farcs2)
    farcs2->next = link, ret = farcs1;
  else
    ret = link;
  return cont;
}

void Fsm::second_action(FsmArcLink* arcs)
{
  int i;
  EventArc* arc = arcs->arc;
  SCM ev = arcs->ev;
  if (((i = WL_EVENTPAT(arc->event)->Index()) == 1 || i == 2 || i == 5) &&
      ((PressReleaseEventArc*)arc)->action2 != SCM_UNDEFINED)
    gwm_apply2_catch(((PressReleaseEventArc*)arc)->action2, wob->scm(), ev);
  else if ((i == 3 || i == 6) && arc->action != SCM_UNDEFINED)
    gwm_apply2_catch(arc->action, wob->scm(), ev);
  if (arcs->next)
    arcs->next->fsm->second_action(arcs->next);
}

/* grab management
 */

struct GrabEle {
  GrabEle(Decoration* w, int key, int cld, int cnf, int nfr, int asc, unsigned int m, WlCursor* cur) 
    { wob = w; top = w->Top(); entered = 0; shadow_entered = 0; shared = 0;
      keys = key; child = cld; confine = cnf; nofreeze = nfr; async = asc;
      mask = m; cursor = cur; next = 0; };
  Decoration* wob;
  Decoration* top;
  Decoration* entered;
  Decoration* shadow_entered;
  int shared;
  int keys;
  int child;
  int confine;
  int nofreeze;
  int async;
  unsigned int mask;
  WlCursor* cursor;
  GrabEle* next;
};

GrabEle* Fsm::GrabStack = 0;
static Decoration* last_grab = 0;
static int last_kbd = 0;
static int last_cnf = 0;
static int last_freeze = 0;
static unsigned int last_mask = 0;
static WlCursor* last_cur = 0;

void Fsm::UpdateGrab()
{
  GrabEle* tmp;
  int status, frz;
  if (!GrabStack) {
    if (last_grab) {
      DoResizeClients();
      XUngrabPointer(dpy, CurrentTime);
      if (last_kbd)
        XUngrabKeyboard(dpy, CurrentTime);
      if (last_freeze && GWM_FreezeServer)
        XUngrabServer(dpy);
      XSync(dpy, 0);
      last_grab = 0;
      last_kbd = 0;
      last_cnf = 0;
      last_freeze = 0;
      last_mask = 0;
      last_cur = 0;
      CatchUngrabRelease();
    }
  } else {
    if (last_grab != GrabStack->wob || last_cnf != GrabStack->confine || 
        last_mask != GrabStack->mask || last_cur != GrabStack->cursor) {
      status = XGrabPointer(dpy, GrabStack->wob->Xwin(), True,
                            GrabStack->mask & 0x7ffc,  // Pointer related bits
                            GrabModeAsync, GrabModeAsync,
                            (GrabStack->confine ? GrabStack->wob->Xwin() : None),
                            (GrabStack->cursor == 0 ? None : GrabStack->cursor->cursor),
                            CurrentTime);
      if (status != GrabSuccess) {
        gwm_warning("Grabbing pointer failed", 0);
        XUngrabPointer(dpy, CurrentTime);
      }
      last_cnf = GrabStack->confine;
      last_mask = GrabStack->mask;
      last_cur = GrabStack->cursor;
    }
    if (last_kbd != GrabStack->keys || (last_grab != GrabStack->wob && GrabStack->keys)) {
      if (GrabStack->keys) {
        status = XGrabKeyboard(dpy, GrabStack->wob->Xwin(), True,
                               GrabModeAsync, GrabModeAsync, CurrentTime);
        if (status != GrabSuccess) {
          gwm_warning("Grabbing keyboard failed", 0);
          XUngrabKeyboard(dpy, CurrentTime);
        }
      } else 
        XUngrabKeyboard(dpy, CurrentTime);
      last_kbd = GrabStack->keys;
    }
    frz = 0;
    for (tmp=GrabStack; tmp; tmp=tmp->next)
      if (!tmp->nofreeze) {
        frz = 1;
        break;
      }
    if (frz) {
      if (!last_freeze && GWM_FreezeServer)
        XGrabServer(dpy);
      last_freeze |= 1;
    } else {
      if (last_freeze == 1 && GWM_FreezeServer)
        XUngrabServer(dpy);
      last_freeze &= ~1;
    }
    last_grab = GrabStack->wob;
    CatchUngrabRelease();
  }
}


GrabEle* Fsm::LocateGrab(Decoration* w, GrabEle*& e1, GrabEle*& e2, int asc)
{
  GrabEle *t1, *t2, *e0;
  t1 = GrabStack;
  t2 = 0;
  while (t1 && (t1->wob != w || t1->async != asc)) {
    t2 = t1;
    t1 = t1->next;
  }
  e1 = t2;
  e0 = t1;
  t2 = t1;
  if (t1) t2 = t1, t1 = t1->next;
  while (t1 && t1->shared > e0->shared) {
    t2 = t1;
    t1 = t1->next;
  }
  e2 = t2;
  return e0;
}

void Fsm::SetGrab(Decoration* gpar, unsigned int smask, int key, int cld, int cnf, int nfr, int asc, WlCursor* cur)
{
  GrabEle *ele, *e1, *e2, *p;
  Decoration** dp1 = (GrabStack ? &GrabStack->entered : &entered_deco);
  Decoration* dp2;
  int diff;
  if (!gpar || gpar == wob) {
    if (asc && (ele = LocateGrab(wob, e1, e2, asc))) {
      if (e1) {
        e1->next = e2->next;
        e2->next = GrabStack;
        GrabStack = ele;
      }
    } else {
      ele = new GrabEle(wob, key, cld, cnf, nfr, asc, (smask ? smask : mask), cur);
      ele->next = GrabStack;
      GrabStack = ele;
      dp2 = (*dp1 ? FindGrab(*dp1, 0) : 0);
      if (dp2 == *dp1 || (cld && IsAnAncestor(dp2, *dp1)) || IsAnAncestor(*dp1, dp2)) {
        GrabStack->entered = *dp1;
        *dp1 = 0;
      }
    }
  } else {
    if (asc && (ele = LocateGrab(wob, e1, e2, asc))) {
      if (e1)
        e1->next = e2->next;
      else
        GrabStack = e2->next;
      e2->next = 0;
    } else
      ele = new GrabEle(wob, key, cld, cnf, nfr, asc, (smask ? smask : mask), cur);
    p = LocateGrab(gpar, e1, e2, asc);
    if (p) {
      diff = p->shared - ele->shared + 1;
      e1 = ele;
      while (e1) {
        e1->shared += diff;
        e1 = e1->next;
      }
      ele->next = p->next;
      p->next = ele;
    } else {
      while (ele) {
        e2 = ele;
        ele = ele->next;
        delete e2;
      }
    }
  }
  UpdateGrab();
}

XCrossingEvent* FakeEnterEvent(Decoration* d)
{
  static XCrossingEvent evt;
  static int init = 0;
  if (!init) {
    evt.type = EnterNotify;
    evt.display = 0;
    evt.window = 0;
    evt.root = 0;
    evt.time = 0;
    evt.x = 0;
    evt.y = 0;
    evt.x_root = 0;
    evt.y_root = 0;
    evt.mode = NotifyNormal;
    evt.detail = NotifyNonlinear;
    init = 1;
  }
  return &evt;
}

void Fsm::RemoveGrab(int asc)
{
  GrabEle *ele, *e1, *e2;
  Decoration* dp = 0;
  ele = LocateGrab(wob, e1, e2, asc);
  if (ele) {
    if (ele == GrabStack && ele->shadow_entered != 0)
      dp = ele->shadow_entered;
    if (e1)
      e1->next = e2->next;
    else
      GrabStack = e2->next;
    e2->next = 0;
    while (ele) {
      e2 = ele;
      ele = ele->next;
      delete e2;
    }
  }
  UpdateGrab();
  if (dp)
    Fsm::register_enter(FakeEnterEvent(dp), dp);
}

void Fsm::ClearGrabs()
{
  GrabEle *e1, *e2;
  Decoration* dp = 0;
  e1 = GrabStack;
  if (e1 && e1->shadow_entered)
    dp = e1->shadow_entered;
  while (e1) {
    e2 = e1;
    e1 = e1->next;
    delete e2;
  }
  GrabStack = 0;
  UpdateGrab();
  if (dp)
    Fsm::register_enter(FakeEnterEvent(dp), dp);
}

Decoration* Fsm::FindGrab(Decoration* w, int gk)
{
  GrabEle *ele;
  Decoration* top = w->Top();
  Decoration *tmp;
  GrabEle *beste = 0;
  int c, bestc = 0;
  if (!GrabStack)
    return 0;
  if (gk && !GrabStack->keys) // MORE gk NEEDED BELOW ****
    return 0;
  if (GrabStack->wob == w)
    return w;
  if (top == GrabStack->top) {
    for (c=0, tmp=w; tmp && GrabStack->wob != tmp; c++, tmp=tmp->Parent());
    if (tmp && (!bestc || c<bestc))
      beste = GrabStack, bestc = c;
  }
  ele = GrabStack->next;
  while (ele && ele->shared) {
    if (ele->wob == w)
      return w;
    if (top == ele->top) {
      for (c=0, tmp=w; tmp && ele->wob != tmp; c++, tmp=tmp->Parent());
      if (tmp && (!bestc || c<bestc))
        beste = ele, bestc = c;
    }
    ele = ele->next;
  }
  if (beste) {
    if (beste->child)
      return beste->wob;
    else
      return w;
  }
  return GrabStack->wob;
}

int Fsm::ServerGrabbed()
{
  return (GrabStack != 0);
}

void Fsm::TempFreezeOn()
{
  if (GrabStack != 0) {
    if (!last_freeze && GWM_FreezeServer)
      XGrabServer(dpy);
    last_freeze |= 2;
  }
}

void Fsm::TempFreezeOff()
{
  if (GrabStack != 0) {
    if (last_freeze == 2 && GWM_FreezeServer)
      XUngrabServer(dpy);
    last_freeze &= ~2;
  }
}

Decoration* Fsm::GetGrabTarget(Decoration* wob, XEvent* evt)
{
  Decoration* w;
  if (event_is_grabbable(evt)) {
    w = FindGrab(wob, evt->type == KeyPress || evt->type == KeyRelease ? 1 : 0);
    return (w ? w : wob);
  } else {
    return wob;
  }
}

void Fsm::register_enter(XCrossingEvent* ev, Decoration* to)
{
  Decoration* from;
  Decoration* cp;
  if (GrabStack) {
    from = GrabStack->entered;
    cp = FindGrab(to, 0);
    if (cp != to) {
      GrabStack->shadow_entered = to;
      if (GrabStack->child && IsAnAncestor(cp, to))
        to = cp;
      else if (!IsAnAncestor(to, cp))
        to = 0;
    } else
      GrabStack->shadow_entered = 0;
  } else {
    from = entered_deco;
  }
  if (from && to) {
    cp = CommonParent(from, to);
    if (cp == to) {
      if (ev->mode == NotifyGrab && GrabStack && !GrabStack->child)
        return;
      if (ev->mode == NotifyUngrab && (ev->detail == NotifyVirtual || ev->detail == NotifyNonlinearVirtual))
        return;
    }
  } else {
    cp = 0;
  }
  while (from && from != cp) {
    from->issue_leave_event(ev);
    from = from->Parent();
  }
  if (cp != to)
    to->issue_enter_recursive(cp, ev);
  if (GrabStack)
    GrabStack->entered = to;
  else
    entered_deco = to;
}

void init_scm_fsm()
{
  scm_tc16_behavior = scm_make_smob_type("behavior", 0);
  scm_set_smob_mark(scm_tc16_behavior, mark_behavior);
  scm_set_smob_free(scm_tc16_behavior, free_behavior);
  scm_set_smob_print(scm_tc16_behavior, print_behavior);
  scm_tc16_eventarc = scm_make_smob_type("eventarc", 0);
  scm_set_smob_mark(scm_tc16_eventarc, mark_eventarc);
  scm_set_smob_free(scm_tc16_eventarc, free_eventarc);
  scm_set_smob_print(scm_tc16_eventarc, print_eventarc);
#include "fsm.x"
}
