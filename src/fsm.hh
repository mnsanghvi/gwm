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










