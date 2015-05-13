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


extern long scm_tc16_wlevent;
extern long scm_tc16_eventpat;

class WlEvent {
public:
  WlEvent(XEvent* e, class Decoration* wob);
  XEvent* event() { return &ev; };
  int type() { return ev.type; };
  int has_state();
  int has_pos();
  int state();
  int relx();
  int rely();
  int rootx();
  int rooty();
protected:
  XEvent ev;
};

class EventPat {
public:
  EventPat() {};
  virtual int Index() = 0; 		// the X type of the event
  virtual unsigned int Mask() = 0;	// the XSelectInput mask
  virtual unsigned int GwmMask() { return 0; };	// the Gwm event mask
  virtual int match(XEvent* ev) { return TRUE; };  
  virtual int equal(EventPat* ep) { return (Index() == ep->Index()); }; 
  virtual void set_grab(Window win) {};
};

class EventPatButtonT : public EventPat {
public:
  EventPatButtonT(int b, int s) { button = b; state = s; };
  virtual int Index() = 0;
  virtual unsigned int Mask() = 0;
  virtual int match2(XEvent* ev, int rel);  
  virtual int equal(EventPat* ep) { return (Index() == ep->Index() && eq((EventPatButtonT*)ep)); }; 
  virtual void set_grab(Window win);
  int eq(EventPatButtonT* p) { return (p->button == button && p->state == state); };
  int lt(EventPatButtonT* p) { return (button == -1 ? p->button != -1 || state == -1 && p->state != -1 : p->button != -1 && state == -1 && p->state != -1); };
protected:
  int button;
  int state;
};

class EventPatButtonpress : public EventPatButtonT {
public:
  EventPatButtonpress(int b, int s) : EventPatButtonT(b, s) {};
  virtual int Index() { return 0; };
  virtual unsigned int Mask() { return ButtonPressMask; };
};

class EventPatButton : public EventPatButtonT {
public:
  EventPatButton(int b, int s) : EventPatButtonT(b, s) {};
  virtual int Index() { return 2; };
  virtual unsigned int Mask() { return ButtonPressMask | ButtonReleaseMask; };
};

class EventPatMultibutton : public EventPatButtonT {
public:
  EventPatMultibutton(int b, int s, int n) : EventPatButtonT(b, s) { num = n; };
  virtual int Index() { return 1; };
  virtual unsigned int Mask() { return ButtonPressMask | ButtonReleaseMask; };
  virtual int match3(XEvent* ev, int rel, int mb);  
  int eq(EventPatMultibutton* p) { return (p->num == num && p->button == button && p->state == state); };
  int lt(EventPatMultibutton* p) { return (p->num > num || p->num == num && (button == -1 ? p->button != -1 || state == -1 && p->state != -1 : p->button != -1 && state == -1 && p->state != -1)); };
  int Num() { return num; };
protected:
  int num;
};

class EventPatButtonrelease : public EventPatButtonT {
public:
  EventPatButtonrelease(int b, int s) : EventPatButtonT(b, s) {};
  virtual int Index() { return 3; };
  virtual unsigned int Mask() { return ButtonReleaseMask; };
  virtual int match2(XEvent* ev, int rel);  
};

class EventPatKeyT : public EventPat {
public:
  EventPatKeyT(int c, int s) { code = c; state = s; };
  virtual int Index() = 0;
  virtual unsigned int Mask() = 0;
  virtual int match2(XEvent* ev, int rel);  
  virtual int equal(EventPat* ep) { return (Index() == ep->Index() && eq((EventPatKeyT*)ep)); }; 
  virtual void set_grab(Window win);
  int eq(EventPatKeyT* p) { return (p->code == code && p->state == state); };
  int lt(EventPatKeyT* p) { return (code == -1 ? p->code != -1 || state == -1 && p->state != -1 : p->code != -1 && state == -1 && p->state != -1); };
protected:
  int code;
  int state;
};

class EventPatKeypress : public EventPatKeyT {
public:
  EventPatKeypress(int c, int s) : EventPatKeyT(c, s) {};
  virtual int Index() { return 4; };
  virtual unsigned int Mask() { return KeyPressMask; };
};

class EventPatKey : public EventPatKeyT {
public:
  EventPatKey(int c, int s) : EventPatKeyT(c, s) {};
  virtual int Index() { return 5; };
  virtual unsigned int Mask() { return KeyPressMask | KeyReleaseMask; };
};

class EventPatKeyrelease : public EventPatKeyT {
public:
  EventPatKeyrelease(int c, int s) : EventPatKeyT(c, s) {};
  virtual int Index() { return 6; };
  virtual unsigned int Mask() { return KeyReleaseMask; };
};

class EventPatMovement : public EventPat {
public:
  EventPatMovement() {};
  virtual int Index() { return 7; };
  virtual unsigned int Mask() { return PointerMotionMask; };
  virtual int match(XEvent* ev);  
};

class EventPatEnter : public EventPat {
public:
  EventPatEnter() {};
  virtual int Index() { return 8; };
  virtual unsigned int Mask() { return 0; };
  virtual unsigned int GwmMask() { return GWMEnterMask; };
  virtual int match(XEvent* ev);  
};

class EventPatLeave : public EventPat {
public:
  EventPatLeave() {};
  virtual int Index() { return 9; };
  virtual unsigned int Mask() { return 0; };
  virtual unsigned int GwmMask() { return GWMLeaveMask; };
  virtual int match(XEvent* ev);  
};

class EventPatFocusin : public EventPat {
public:
  EventPatFocusin() {};
  virtual int Index() { return 10; };
  virtual unsigned int Mask() { return FocusChangeMask; };
  virtual int match(XEvent* ev);  
};

class EventPatFocusout : public EventPat {
public:
  EventPatFocusout() {};
  virtual int Index() { return 11; };
  virtual unsigned int Mask() { return FocusChangeMask; };
  virtual int match(XEvent* ev);  
};

class EventPatMap : public EventPat {
public:
  EventPatMap() {};
  virtual int Index() { return 12; };
  virtual unsigned int Mask() { return 0; };
  virtual unsigned int GwmMask() { return GWMMapMask; };
};

class EventPatUnmap : public EventPat {
public:
  EventPatUnmap() {};
  virtual int Index() { return 13; };
  virtual unsigned int Mask() { return 0; };
  virtual unsigned int GwmMask() { return GWMUnmapMask; };
};

class EventPatResize : public EventPat {
public:
  EventPatResize() {};
  virtual int Index() { return 14; };
  virtual unsigned int Mask() { return 0; };
  virtual unsigned int GwmMask() { return GWMResizeMask; };
};

class EventPatMove : public EventPat {
public:
  EventPatMove() {};
  virtual int Index() { return 15; };
  virtual unsigned int Mask() { return 0; };
  virtual unsigned int GwmMask() { return GWMMoveMask; };
};

class EventPatStack : public EventPat {
public:
  EventPatStack() {};
  virtual int Index() { return 16; };
  virtual unsigned int Mask() { return 0; };
  virtual unsigned int GwmMask() { return GWMStackMask; };
};

class EventPatUser : public EventPat {
public:
  EventPatUser(SCM val) { value = val; };
  virtual int Index() { return 17; };
  virtual unsigned int Mask() { return 0; };
  virtual unsigned int GwmMask() { return GWMUserMask; };
  virtual int match(XEvent* ev);  
  virtual int equal(EventPat* ep) { return (Index() == ep->Index() && eq((EventPatUser*)ep)); }; 
  int eq(EventPatUser* p) { return (p->value == value); };
  void mark() { if (value) scm_gc_mark(value); };
protected:
  SCM value;
};

class EventPatPropertychange : public EventPat {
public:
  EventPatPropertychange(Atom p) { prop = p; };
  virtual int Index() { return 18; };
  virtual unsigned int Mask() { return PropertyChangeMask; };
  virtual int match(XEvent* ev);  
  virtual int equal(EventPat* ep) { return (Index() == ep->Index() && eq((EventPatPropertychange*)ep)); }; 
  int eq(EventPatPropertychange* p) { return (p->prop == prop); };
protected:
  Atom prop;
};

class EventPatIconchange : public EventPatPropertychange {
public:
  EventPatIconchange() : EventPatPropertychange(0) {};
  virtual int Index() { return 18; };
  virtual unsigned int Mask() { return PropertyChangeMask; };
  virtual int match(XEvent* ev);  
};

class EventPatClientmessage : public EventPat {
public:
  EventPatClientmessage(Atom p) { prop = p; };
  virtual int Index() { return 19; };
  virtual unsigned int Mask() { return SubstructureNotifyMask; };
  virtual int match(XEvent* ev);  
  virtual int equal(EventPat* ep) { return (Index() == ep->Index() && eq((EventPatClientmessage*)ep)); }; 
  int eq(EventPatClientmessage* p) { return (p->prop == prop); };
protected:
  Atom prop;
};

class EventPatOpening : public EventPat {
public:
  EventPatOpening() {};
  virtual int Index() { return 20; };
  virtual unsigned int Mask() { return 0; };
  virtual unsigned int GwmMask() { return GWMOpeningMask; };
};

class EventPatClosing : public EventPat {
public:
  EventPatClosing() {};
  virtual int Index() { return 21; };
  virtual unsigned int Mask() { return 0; };
  virtual unsigned int GwmMask() { return GWMClosingMask; };
};

struct TimerEvent {
  TimerEvent(XEvent* ev, class Decoration* w, int t) { event = ev; wob = w; time = t; next = 0; scm_gc_protect_object(wob->scm()); };
  ~TimerEvent() { scm_gc_unprotect_object(wob->scm()); delete event; };
  XEvent* event;
  Decoration* wob;
  int time;
  TimerEvent* next;
};

struct GwmEvent {
  GwmEvent(XEvent* ev, class Decoration* w, GwmEvent* ge) { event = ev; wob = w; count = 0; cause = ge; if (cause) cause->count++; next = 0; scm_gc_protect_object(wob->scm()); };
  ~GwmEvent() { scm_gc_unprotect_object(wob->scm()); if (cause) cause->count--; delete event; };
  XEvent* event;
  Decoration* wob;
  int count;
  GwmEvent* cause;
  GwmEvent* next;
};

Time fetch_event_time(XEvent* ev);
unsigned int fetch_event_mask(XEvent* ev);
unsigned int fetch_event_gwmmask(XEvent* ev);
int fetch_event_index(XEvent* ev);
int event_is_grabbable(XEvent* ev);
SCM event2scm(WlEvent* event);
SCM eventpat2scm(EventPat* event);
int check_bounced_event(XEvent* evt);
int count_multibutton(XEvent* evt, int maxm, int& rsflag, int& half);
void resend_multibutton(XEvent* evt, int mb, int half, int& rsflag);
int WaitForButtonRelease(unsigned int button, Window win, XEvent* event, int poll, int timeout);
void send_gwm_event(class Decoration* w, int type);
void send_gwm_crossing_event(class Decoration* w, XCrossingEvent* ev, int type);
void EnqueueTimerEvent(TimerEvent* te);
int NextTimerDelay();
void ProcessGwmEvents();
void ProcessTimerEvents();
void SetupReleaseGuard(XEvent* evt, class Decoration* deco, class FsmArcLink* arcs, int resent, int half);
void RegisterReleaseRedir(XEvent* evt);
int RegisterRelease(XEvent* evt, int& rsflag);
XEvent* CheckAnyRelease();
void CatchNormalRelease(XEvent* evt);
void CatchUnmapRelease(class Decoration* deco);
void CatchUngrabRelease();

extern GwmEvent* gwmevent_current;

#define WLEVENTP(x) (SCM_NIMP(x) && SCM_CAR(x) == (SCM)scm_tc16_wlevent)
#define WL_EVENT(x) ((WlEvent*) SCM_CDR(x))
#define WLEVENTPATP(x) (SCM_NIMP(x) && SCM_CAR(x) == (SCM)scm_tc16_eventpat)
#define WL_EVENTPAT(x) ((EventPat*) SCM_CDR(x))
#define WL_EVENTPATBUTTON(x) ((EventPatButtonT*) SCM_CDR(x))
#define WL_EVENTPATMULTIBUTTON(x) ((EventPatMultibutton*) SCM_CDR(x))
#define WL_EVENTPATKEY(x) ((EventPatKeyT*) SCM_CDR(x))
#define WL_EVENTPATUSER(x) ((EventPatUser*) SCM_CDR(x))
#define WL_EVENTPATPROPERTYCHANGE(x) ((EventPatPropertychange*) SCM_CDR(x))
#define WL_EVENTPATCLIENTMESSAGE(x) ((EventPatClientmessage*) SCM_CDR(x))

#define REL_OCCURRED 1
#define REL_RESEND 2
#define REL_RESEND_SYNT 4 
#define REL_GRABWAIT 8
#define REL_REDIRECTED 16

#define RESENT_NORMAL 1
#define RESENT_SYNT 2
#define RESENT_BOUNCED 3
















































































