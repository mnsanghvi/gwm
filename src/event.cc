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


 /***********************\
 * 		         *
 *  Event Objects        *
 * 		         *
 \***********************/

#include <guile/gh.h>

#include "gwm.hh"
#include "gwmfunc.hh"
#include "error.hh"
#include "cursor.hh"
#include "deco.hh"
#include "screen.hh"
#include "client.hh"
#include "fsm.hh"
#include "event.hh"

/*
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

*/

long scm_tc16_wlevent;   
long scm_tc16_eventpat;

SCM v_movement;
SCM v_enter;
SCM v_leave;
SCM v_focus_in;
SCM v_focus_out;
SCM v_map_event;
SCM v_unmap_event;
SCM v_resize_event;
SCM v_move_event;
SCM v_stack_event;
SCM v_name_change;
SCM v_window_icon_pixmap_change;
SCM v_opening_event;
SCM v_closing_event;

/* request error codes */

char *event_name[] = {
  "ButtonPress",
  "MultiButton",
  "Button",
  "ButtonRelease",
  "KeyPress",
  "Key",
  "KeyRelease",
  "Movement",
  "Enter",
  "Leave",
  "FocusIn",
  "FocusOut",
  "MapEvent",
  "UnmapEvent",
  "ResizeEvent",
  "MoveEvent",
  "StackEvent",
  "UserEvent",
  "PropertyChange",
  "ClientMessage",
  "Opening",
  "Closing"
};

/* to filter button releases events from the fact that they get the same button
 * pressed as a modifier...
 */
unsigned int ButtonMasks[] = {
    0,
    Button1Mask,
    Button2Mask,
    Button3Mask,
    Button4Mask,
    Button5Mask
};

Time fetch_event_time(XEvent* ev)
{
  switch (ev->type) {
  case KeyPress: 
  case KeyRelease: return ev->xkey.time;
  case ButtonPress:
  case ButtonRelease: return ev->xbutton.time;
  case MotionNotify: return ev->xmotion.time;
  case GWMEnterEvent:
  case GWMLeaveEvent:
  case EnterNotify:
  case LeaveNotify: return ev->xcrossing.time;
  case PropertyNotify: return ev->xproperty.time;
  case SelectionClear: return ev->xselectionclear.time;
  case SelectionRequest: return ev->xselectionrequest.time;
  case SelectionNotify: return ev->xselection.time;
  default: return 0;
  }
}

unsigned int fetch_event_mask(XEvent* ev)
{
  switch (ev->type) {
  case KeyPress: return KeyPressMask;
  case KeyRelease: return KeyReleaseMask;
  case ButtonPress: return ButtonPressMask;
  case ButtonRelease: return ButtonReleaseMask;
  case MotionNotify: return PointerMotionMask;
  case EnterNotify: return EnterWindowMask;
  case LeaveNotify: return LeaveWindowMask;
  case FocusIn:
  case FocusOut: return FocusChangeMask;
  case KeymapNotify: return KeymapStateMask;
  case Expose:
  case GraphicsExpose:
  case NoExpose: return ExposureMask;
  case VisibilityNotify: return VisibilityChangeMask;
  case CreateNotify: 
  case DestroyNotify:
  case UnmapNotify:
  case MapNotify:
  case ReparentNotify: 
  case ConfigureNotify:
  case GravityNotify:
  case ClientMessage:
  case CirculateNotify: return StructureNotifyMask | SubstructureNotifyMask;
  case MapRequest:
  case ConfigureRequest:
  case ResizeRequest:
  case CirculateRequest: return ResizeRedirectMask | SubstructureRedirectMask;
  case PropertyNotify : return PropertyChangeMask;
  case ColormapNotify : return ColormapChangeMask;
  default: return 0;
  }
}

unsigned int fetch_event_gwmmask(XEvent* ev)
{
  switch (ev->type) {
  case GWMMapEvent: return GWMMapMask;
  case GWMUnmapEvent: return GWMUnmapMask;
  case GWMResizeEvent: return GWMResizeMask;
  case GWMMoveEvent: return GWMMoveMask;
  case GWMStackEvent: return GWMStackMask;
  case GWMUserEvent: return GWMUserMask;
  case GWMEnterEvent: return GWMEnterMask;
  case GWMLeaveEvent: return GWMLeaveMask;
  case GWMFocusinEvent: return GWMFocusinMask;
  case GWMFocusoutEvent: return GWMFocusoutMask;
  case GWMOpeningEvent: return GWMOpeningMask;
  case GWMClosingEvent: return GWMClosingMask;
  default: return 0;
  }
}

int fetch_event_index(XEvent* ev)
{
  switch (ev->type) {
  case KeyPress: return 4;
  case KeyRelease: return 6;
  case ButtonPress: return 0;
  case ButtonRelease: return 3;
  case MotionNotify: return 7;
  case GWMEnterEvent: return 8;
  case GWMLeaveEvent: return 9;
  case FocusIn: return 10;
  case FocusOut: return 11;
  case GWMMapEvent: return 12;
  case GWMUnmapEvent: return 13;
  case GWMResizeEvent: return 14;
  case GWMMoveEvent: return 15;
  case GWMStackEvent: return 16;
  case GWMUserEvent: return 17;
  case PropertyNotify: return 18;
  case ClientMessage: return 19;
  case GWMOpeningEvent: return 20;
  case GWMClosingEvent: return 21;
  default: return -1;
  }
}

int event_is_grabbable(XEvent* ev)
{
  switch (ev->type) {
  case KeyPress: 
  case KeyRelease:
  case ButtonPress:
  case ButtonRelease:
  case MotionNotify: return 1;
  default: return 0;
  }
}

SCM mark_wlevent(SCM obj)
{
  return SCM_BOOL_F;
}

size_t free_wlevent(SCM obj)
{
  delete WL_EVENT(obj);
  return 0;
};

int print_wlevent(SCM obj, SCM port, scm_print_state * pstate)
{
  int i = fetch_event_index(WL_EVENT(obj)->event());
  scm_puts("#<xevent: ", port);
  scm_puts((i<0 ? "" : event_name[i]), port);
  scm_puts(">", port);
  return 1;
};

SCM event2scm(WlEvent* event)
{
  return scm_cell((scm_t_bits) scm_tc16_wlevent, (scm_t_bits) event);
}

Decoration* GetTargetWob(XEvent* evt)
{
  Decoration* wob;
  ClientWindow* cw;
  if ((wob = LookUpDeco(evt->xany.window))) {
    if (Fsm::ServerGrabbed())
      wob = Fsm::GetGrabTarget(wob, evt);
    if (!wob || wob->Screen() != Context)
      return 0;
    else
      return wob;
  } else if ((cw = LookUpClient(evt->xany.window))) {
    if (!cw->Inner() || cw->Inner()->Screen() != Context)
      return 0;
    else
      return cw->Inner();
  } else
    return 0;
}
    
SCM_DEFINE(wl_event_p, "xevent?", 1, 0, 0,
           (SCM obj),
           "Return true if obj is an X-event.")
{
  return (WLEVENTP(obj) ? SCM_BOOL_T : SCM_BOOL_F);
}

WlEvent::WlEvent(XEvent* e, Decoration* wob)
{
  int x, y;
  bcopy(e, &ev, sizeof(XEvent));
  if (!wob)
    wob = GetTargetWob(e);
  if (!wob) {
    ev.xany.window = 0;
  } else if (ev.xany.window != wob->Xwin()) {
    ev.xany.window = wob->Xwin();
    if (has_pos()) {
      if (ev.xbutton.root == wob->Screen()->Root()) {
        x = wob->Xpos();
        y = wob->Ypos();
        while (wob->Parent()) {
          wob = wob->Parent();
          if (!(wob->Type() & InternMenuStatus)) {
            x += wob->Borderwidth() + wob->Xpos();
            y += wob->Borderwidth() + wob->Ypos();
          }
        }
        ev.xbutton.x = ev.xbutton.x_root - x;
        ev.xbutton.y = ev.xbutton.y_root - y;
      } else {
        ev.xbutton.x = 0;
        ev.xbutton.y = 0;
      }
    }
  }
}

int WlEvent::has_state()
{
  switch (ev.type) {
  case KeyPress: 
  case KeyRelease: 
  case ButtonPress: 
  case ButtonRelease: 
  case MotionNotify: return 1;
  default: return 0;
  }
}

int WlEvent::has_pos()
{
  switch (ev.type) {
  case KeyPress: 
  case KeyRelease: 
  case ButtonPress: 
  case ButtonRelease: 
  case MotionNotify: 
  case GWMEnterEvent: 
  case GWMLeaveEvent: return 1;
  default: return 0;
  }
}

int WlEvent::state()
{
  return ev.xbutton.state;
}

int WlEvent::relx()
{
  return ev.xbutton.x;
}

int WlEvent::rely()
{
  return ev.xbutton.y;
}

int WlEvent::rootx()
{
  return ev.xbutton.x_root;
}

int WlEvent::rooty()
{
  return ev.xbutton.y_root;
}

SCM mark_eventpat(SCM obj)
{
  if (WL_EVENTPAT(obj)->Index() == 17)
    WL_EVENTPATUSER(obj)->mark();
  return SCM_BOOL_F;
}

size_t free_eventpat(SCM obj)
{
  delete WL_EVENTPAT(obj);
  return 0;
};

int print_eventpat(SCM obj, SCM port, scm_print_state * pstate)
{
  scm_puts("#<event: ", port);
  scm_puts(event_name[WL_EVENTPAT(obj)->Index()], port);
  scm_puts(">", port);
  return 1;
};

SCM equal_eventpat(SCM e1, SCM e2)
{
  return (WL_EVENTPAT(e1)->equal(WL_EVENTPAT(e2)) ? SCM_BOOL_T : SCM_BOOL_F);
}

SCM eventpat2scm(EventPat* event)
{
  return scm_cell((scm_t_bits) scm_tc16_eventpat, (scm_t_bits) event);
}

SCM_DEFINE(wl_eventpat_p, "event?", 1, 0, 0,
           (SCM obj),
           "Return true if obj is an event descriptor.")
{
  return (WLEVENTPATP(obj) ? SCM_BOOL_T : SCM_BOOL_F);
}


static int global_found_press = 0;

int LookForButtonPressSameWindow(Display* display, XEvent* evt, XPointer arg)
{
  XButtonEvent* origevt = (XButtonEvent*)arg;
  if (evt->type == ButtonPress) {
    if (evt->xbutton.window != origevt->window) {
      global_found_press = 1;
      return False;
    } else if (!global_found_press && evt->xbutton.button == origevt->button)
      return True;
  }
  return False;
}

int LookForButtonRelease(Display* display, XEvent* evt, XPointer arg)
{
  XButtonEvent* origevt = (XButtonEvent*)arg;
  if ((evt->type == ButtonRelease) &&
      (evt->xbutton.button == origevt->button))
    return True;
  else
    return False;
}

int LookForButtonReleaseOrPress(Display* display, XEvent* evt, XPointer arg)
{
  XButtonEvent* origevt = (XButtonEvent*)arg;
  if ((evt->type == ButtonPress ||
       evt->type == ButtonRelease) &&
      (evt->xbutton.button == origevt->button)) {
    if (evt->type == ButtonPress)
      global_found_press = 1;
    return True;
  } else
    return False;
}

int LookForButtonReleaseNoPress(Display* display, XEvent* evt, XPointer arg)
{
  XButtonEvent* origevt = (XButtonEvent*)arg;
  if ((evt->type == ButtonPress) &&
      (evt->xbutton.button == origevt->button)) {
    global_found_press = 1;
    return False;
  } else if (!global_found_press &&
             (evt->type == ButtonRelease) &&
             (evt->xbutton.button == origevt->button))
    return True;
  else
    return False;
}

int LookForKeyReleaseOrPress(Display* display, XEvent* evt, XPointer arg)
{
  XKeyEvent* origevt = (XKeyEvent*)arg;
  if ((evt->type == KeyPress ||
       evt->type == KeyRelease) &&
      (evt->xkey.keycode == origevt->keycode)) {
    if (evt->type == KeyPress)
      global_found_press = 1;
    return True;
  } else
    return False;
}

int LookForKeyReleaseNoPress(Display* display, XEvent* evt, XPointer arg)
{
  XKeyEvent* origevt = (XKeyEvent*)arg;
  if ((evt->type == KeyPress) &&
      (evt->xkey.keycode == origevt->keycode)) {
    global_found_press = 1;
    return False;
  } else if (!global_found_press &&
             (evt->type == KeyRelease) &&
             (evt->xkey.keycode == origevt->keycode))
    return True;
  else
    return False;
}


int LookForBouncedEvent(Display* display, XEvent* evt, XPointer arg)
{
  XEvent* origevt = (XEvent*)arg;
  if (origevt->type == ButtonPress) {
    return (evt->type == ButtonPress &&
            evt->xbutton.time == origevt->xbutton.time &&
            evt->xbutton.state == origevt->xbutton.state &&
            evt->xbutton.button == origevt->xbutton.button &&
            evt->xbutton.window != origevt->xbutton.window);
  } else if (origevt->type == KeyPress) {
    return (evt->type == KeyPress &&
            evt->xkey.time == origevt->xkey.time &&
            evt->xkey.state == origevt->xkey.state &&
            evt->xkey.keycode == origevt->xkey.keycode &&
            evt->xkey.window != origevt->xkey.window);
  } else
    return 0;
}

int check_bounced_event(XEvent* evt)
{
  XEvent event;
  XSync(dpy, 0);
  if (XCheckIfEvent(dpy, &event, LookForBouncedEvent, (XPointer) evt))
    return 1;
  else
    return 0;
}

/*
 * Matching procedures for each event
 */

extern int GWM_AwaitInput(int maxtime);

int count_multibutton(XEvent* evt, int maxm, int& rsflag, int& half)
{
  int i;
  int t, dt, del, ok;
  XEvent event;
  half = 1;
  if (maxm <= 1) {
    return maxm;
  } else {
    if (rsflag) {
      XAllowEvents(dpy, (rsflag == RESENT_NORMAL ? ReplayPointer : AsyncPointer), CurrentTime);
      if (rsflag == RESENT_NORMAL && check_bounced_event(evt))
        rsflag = RESENT_BOUNCED;
    }
    if (!scm_is_integer(SCM_VARIABLE_REF(v_double_click_delay)))
      return 1;
    del = gh_scm2int(SCM_VARIABLE_REF(v_double_click_delay));
    for (i=1; i<maxm; i++) {
      t = gwm_proper_time() + del;
      dt = del;
      ok = 0;
      while (dt > 0) {
        global_found_press = 0;
        if (XCheckIfEvent(dpy, &event, LookForButtonReleaseNoPress, (XPointer)&evt->xbutton)) {
          half = 0;
          if (rsflag) {
            XAllowEvents(dpy, (rsflag == RESENT_NORMAL ? ReplayPointer : AsyncPointer), CurrentTime);
          }
        }
        global_found_press = 0;
        if (XCheckIfEvent(dpy, &event, LookForButtonPressSameWindow, (XPointer)&evt->xbutton)) {
          half = 1;
          if (rsflag) {
            XAllowEvents(dpy, (rsflag == RESENT_NORMAL ? ReplayPointer : AsyncPointer), CurrentTime);
          }
          ok = 1;
          break;
        } else if (global_found_press)
          break;
        dt = t - gwm_proper_time();
        if (dt > 0)
          GWM_AwaitInput(dt);
      }
      if (!ok)
        break;
    }
    return i;
  }
}

void fill_x_key_event(XKeyPressedEvent* evt, int keycode, int modifier, Window win, Window root);
void fill_x_button_event(XButtonEvent* evt, int button, int modifier, int x, int y, int x_root, int y_root, Window child, Window root);
Window WindowGettingButtonEvent(Window w, int x, int y);

void resend_multibutton(XEvent* evt, int mb, int half, int& rsflag)
{
  int x2, y2;
  XEvent event;
  Window dummy;
  Window win = evt->xbutton.window;
  Window root = evt->xbutton.root;
  Window child = WindowGettingButtonEvent(win, evt->xbutton.x, evt->xbutton.y);
  if (!child) {
    rsflag = RESENT_BOUNCED;
    return;
  }
  XTranslateCoordinates(dpy, win, child, evt->xbutton.x, evt->xbutton.y, &x2, &y2, &dummy);
  fill_x_button_event((XButtonEvent*) &event,
                      evt->xbutton.button, evt->xbutton.state, x2, y2,
                      evt->xbutton.x_root, evt->xbutton.y_root, child, root);
  event.xbutton.time = evt->xbutton.time;
  if (mb) {
    event.type = ButtonPress;
    XSendEvent(dpy, child, False, ButtonPressMask, &event);
    while (--mb) {
      event.type = ButtonRelease;
      XSendEvent(dpy, child, False, ButtonReleaseMask, &event);
      event.type = ButtonPress;
      XSendEvent(dpy, child, False, ButtonPressMask, &event);
    }
  }
  if (half) {
    event.type = ButtonRelease;
    XSendEvent(dpy, child, False, ButtonReleaseMask, &event);
  }
}

int EventPatButtonT::match2(XEvent* evt, int rel)
{
  if ((button == ANY || button == (int) evt->xbutton.button) &&
      (state == ANY || (state & rel) == (int) (evt->xbutton.state & rel)))
    return TRUE;
  else
    return FALSE;
}

int EventPatMultibutton::match3(XEvent* evt, int rel, int mb)
{
  if ((button == ANY || button == (int) evt->xbutton.button) &&
      (state == ANY || (state & rel) == (int) (evt->xbutton.state & rel)) &&
      num == mb)
    return TRUE;
  else
    return FALSE;
}

int EventPatButtonrelease::match2(XEvent* evt, int rel)
{
  unsigned int butmask = (~ButtonMasks[evt->xbutton.button]) & rel;
  if ((button == ANY || button == (int) evt->xbutton.button) &&
      (state == ANY || (int) (state & butmask) == (int) (evt->xbutton.state & butmask)))
    return TRUE;
  else
    return FALSE;
}

void EventPatButtonT::set_grab(Window win)
{
  XGrabButton(dpy, 
              (button == ANY ? AnyButton : button),
              AnyModifier,
              win, FALSE, ButtonPressMask | ButtonReleaseMask,
              GrabModeSync, GrabModeAsync, None, None);
}

int EventPatKeyT::match2(XEvent* evt, int rel)
{
  KeySym keysym;
  int mod;
  if (code < 0) {
    if ((code == ANY || -code == (int) evt->xkey.keycode) &&
	(state == ANY || (state & rel) == (int) (evt->xkey.state & rel)))
      return TRUE;
    else 
      return FALSE;
  } else {
    keysym = keycode_and_modifier_to_keysym(evt->xkey.keycode, evt->xkey.state);
    mod = keysym_to_keycode_modifier_mask(keysym, evt->xkey.keycode);
    if ((code == (int) keysym) &&
	(state == ANY || ((state & rel) | mod) == (int) (evt->xkey.state & (rel | mod))))
      return TRUE;
    else
      return FALSE;
  }
}

void EventPatKeyT::set_grab(Window win)
{
  KeyCode keycode = 0;
  if (code == ANY)
    keycode = AnyKey;
  else if (code < 0)
    keycode = -code;
  else
    keycode = XKeysymToKeycode(dpy, code);
  if (keycode)
    XGrabKey(dpy, keycode,
             AnyModifier,
             win, FALSE, GrabModeAsync, GrabModeSync);
}

static Window EventPat_check_motion_window;
static int EventPat_check_motion_block;

int EventPat_check_motion(Display* d, XEvent* evt, char* arg)
{
  if (EventPat_check_motion_block)
    return 0;
  if (evt->type != MotionNotify) {
    if (evt->type != PropertyNotify && evt->type != ConfigureNotify &&
        evt->type != Expose && evt->type != VisibilityNotify)
      EventPat_check_motion_block = 1;
    return 0;
  }
  return (evt->xmotion.window == EventPat_check_motion_window ? 1 : 0);
}

int EventPat_latest_motion(XEvent* ret, Window win)
{
  int ok = 0;
  EventPat_check_motion_window = win;
  EventPat_check_motion_block = 0;
  XSync(dpy, 0);
  while (XCheckIfEvent(dpy, ret, EventPat_check_motion, 0)) ok=1;
  return ok;
}

int EventPatMovement::match(XEvent* evt)
{
  XEvent ret;
  if (EventPat_latest_motion(&ret, evt->xmotion.window)) {
    evt->xmotion.subwindow = ret.xmotion.subwindow;
    evt->xmotion.time = ret.xmotion.time;
    evt->xmotion.x = ret.xmotion.x; 
    evt->xmotion.y = ret.xmotion.y;
    evt->xmotion.x_root = ret.xmotion.x_root; 
    evt->xmotion.y_root = ret.xmotion.y_root;
    evt->xmotion.state = ret.xmotion.state;
  }
  return TRUE;
}

int EventPatEnter::match(XEvent* evt)
{
  return True;
}

int EventPatLeave::match(XEvent* evt)
{
  return True;
}

int EventPatFocusin::match(XEvent* evt)
{
  if (evt->xfocus.detail == NotifyAncestor ||
      evt->xfocus.detail == NotifyVirtual ||
      evt->xfocus.detail == NotifyNonlinear ||
      evt->xfocus.detail == NotifyNonlinearVirtual)
    return TRUE;
  else
    return FALSE;
}

int EventPatFocusout::match(XEvent* evt)
{
  if (evt->xfocus.detail == NotifyAncestor ||
      evt->xfocus.detail == NotifyVirtual ||
      evt->xfocus.detail == NotifyNonlinear ||
      evt->xfocus.detail == NotifyNonlinearVirtual)
    return TRUE;
  else
    return FALSE;
}

int EventPatUser::match(XEvent* evt)
{
  SCM val = PTR2SCM(evt->xany.display);
  if (gh_pair_p(val) ? value == SCM_CAR(val) : value == val)
    return TRUE;
  else
    return FALSE;
}

int EventPatPropertychange::match(XEvent* evt)
{
  if (prop == (Atom) ANY || prop == evt->xproperty.atom)
    return TRUE;
  else
    return FALSE;
}

int EventPatIconchange::match(XEvent* evt)
{
  ClientWindow* cw;
  XWMHints *h1, *h2;
  if (evt->xproperty.atom == XA_WM_HINTS) {
    cw = LookUpClient(evt->xany.window);
    if (cw) {
      h1 = cw->WmHints();
      h2 = cw->OldWmHints();
      if ((h1->flags & IconPixmapHint) &&
          (!(h2->flags & IconPixmapHint) ||
           h1->icon_pixmap != h2->icon_pixmap))
        return TRUE;
    }
  }
  return FALSE;
}

int EventPatClientmessage::match(XEvent* evt)
{
  if (prop == (Atom) ANY || prop == evt->xclient.message_type)
    return TRUE;
  else
    return FALSE;
}

int WaitForButtonRelease(unsigned int button, Window win, XEvent* event, int poll, int timeout)
{
  unsigned int ptrmask;
  Window root, sub_window;
  int root_x, root_y, cur_x, cur_y;
  int endtime;
  XButtonEvent dummyevent;
  poll = 1; // always poll for now - shit may happen
  dummyevent.button = button;
  if (timeout)
    endtime = gwm_proper_time() + timeout;
  if (poll || timeout)
    while (1) {
      global_found_press = 0;
      if (XCheckIfEvent(dpy, event, LookForButtonReleaseNoPress, (XPointer)&dummyevent)) {
        if (LookUpClient(event->xany.window))  // Inner client - event is grabbed
          XAllowEvents(dpy, AsyncPointer, CurrentTime);
        return 1;
      } else if (global_found_press) {
        XPeekIfEvent(dpy, event, LookForButtonReleaseOrPress, (XPointer)&dummyevent);
	XTranslateCoordinates(dpy, event->xbutton.root, win, event->xbutton.x_root, event->xbutton.y_root, &cur_x, &cur_y, &sub_window);
        event->xbutton.window = win, event->xbutton.x = cur_x, event->xbutton.y = cur_y, event->xbutton.type = ButtonRelease;
        return 1;
      }
      XQueryPointer(dpy, Context->Root(), &root, &sub_window,
                    &root_x, &root_y, &cur_x, &cur_y, &ptrmask);
      if (!(ptrmask & ButtonMasks[button])) {
	XTranslateCoordinates(dpy, root, win, root_x, root_y, &cur_x, &cur_y, &sub_window);
        fill_x_button_event((XButtonEvent*) event, button, ptrmask | ButtonMasks[button],
                            cur_x, cur_y, root_x, root_y, win, root);
        event->xbutton.type = ButtonRelease;
        return 1;
      }
      if (timeout) {
        timeout = endtime - gwm_proper_time();
        if (timeout <= 0)
          return 0;
      }
      GWM_AwaitInput(poll ? 50 : timeout);
    }
  else {
    global_found_press = 0;
    if (XCheckIfEvent(dpy, event, LookForButtonReleaseNoPress, (XPointer)&dummyevent)) {
      if (LookUpClient(event->xany.window))  // Inner client - event is grabbed
        XAllowEvents(dpy, AsyncPointer, CurrentTime);
      return 1;
    } else if (global_found_press) {
      XPeekIfEvent(dpy, event, LookForButtonReleaseOrPress, (XPointer)&dummyevent);
      XTranslateCoordinates(dpy, event->xbutton.root, win, event->xbutton.x_root, event->xbutton.y_root, &cur_x, &cur_y, &sub_window);
      event->xbutton.window = win, event->xbutton.x = cur_x, event->xbutton.y = cur_y, event->xbutton.type = ButtonRelease;
      return 1;
    }
    XQueryPointer(dpy, Context->Root(), &root, &sub_window,
                  &root_x, &root_y, &cur_x, &cur_y, &ptrmask);
    if (!(ptrmask & ButtonMasks[button])) {
      XTranslateCoordinates(dpy, root, win, root_x, root_y, &cur_x, &cur_y, &sub_window);
      fill_x_button_event((XButtonEvent*) event, button, ptrmask | ButtonMasks[button],
                          cur_x, cur_y, root_x, root_y, win, root);
      event->xbutton.type = ButtonRelease;
      return 1;
    }
    global_found_press = 0;
    XPeekIfEvent(dpy, event, LookForButtonReleaseOrPress, (XPointer)&dummyevent);
    if (!global_found_press) {
      if (LookUpClient(event->xany.window))  // Inner client - event is grabbed
        XAllowEvents(dpy, AsyncPointer, CurrentTime);
      XIfEvent(dpy, event, LookForButtonReleaseNoPress, (XPointer)&dummyevent);
    } else {
      XTranslateCoordinates(dpy, event->xbutton.root, win, event->xbutton.x_root, event->xbutton.y_root, &cur_x, &cur_y, &sub_window);
      event->xbutton.window = win, event->xbutton.x = cur_x, event->xbutton.y = cur_y, event->xbutton.type = ButtonRelease;
    }
    return 1;
  }
}

int WaitForKeyRelease(unsigned int keycode, Window win, XEvent* event, int poll, int timeout)
{
  unsigned int ptrmask;
  Window root, sub_window;
  int root_x, root_y, cur_x, cur_y;
  int endtime;
  XKeyEvent dummyevent;
  poll = 1; // always poll for now - shit may happen
  dummyevent.keycode = keycode;
  if (timeout)
    endtime = gwm_proper_time() + timeout;
  if (poll || timeout) {
    char map[32];
    int i = keycode/8;
    int b = 1<<(keycode%8);
    while (1) {
      global_found_press = 0;
      if (XCheckIfEvent(dpy, event, LookForKeyReleaseNoPress, (XPointer)&dummyevent)) {
        if (LookUpClient(event->xany.window))  // Inner client - event is grabbed
          XAllowEvents(dpy, AsyncKeyboard, CurrentTime);
        return 1;
      } else if (global_found_press) {
        XPeekIfEvent(dpy, event, LookForKeyReleaseOrPress, (XPointer)&dummyevent);
	XTranslateCoordinates(dpy, event->xkey.root, win, event->xkey.x_root, event->xkey.y_root, &cur_x, &cur_y, &sub_window);
        event->xkey.window = win, event->xkey.x = cur_x, event->xkey.y = cur_y, event->xkey.type = KeyRelease;
        return 1;
      }
      XQueryKeymap(dpy, map);
      if (!(map[i] & b)) {
        XQueryPointer(dpy, Context->Root(), &root, &sub_window,
                      &root_x, &root_y, &cur_x, &cur_y, &ptrmask);
        XTranslateCoordinates(dpy, root, win, root_x, root_y, &cur_x, &cur_y, &sub_window);
        fill_x_key_event((XKeyEvent*) event, keycode, ptrmask, win, root);
        event->xkey.x = cur_x, event->xkey.y = cur_y, event->xkey.x_root = root_x, event->xkey.y_root = root_y;
        event->xkey.type = KeyRelease;
        return 1;
      }
      if (timeout) {
        timeout = endtime - gwm_proper_time();
        if (timeout <= 0)
          return 0;
      }
      GWM_AwaitInput(poll ? 50 : timeout);
    }
  } else {
    global_found_press = 0;
    XPeekIfEvent(dpy, event, LookForKeyReleaseOrPress, (XPointer)&dummyevent);
    if (!global_found_press) {
      XIfEvent(dpy, event, LookForKeyReleaseNoPress, (XPointer)&dummyevent);
      if (LookUpClient(event->xany.window))  // Inner client - event is grabbed
        XAllowEvents(dpy, AsyncKeyboard, CurrentTime);
    } else {
      XTranslateCoordinates(dpy, event->xkey.root, win, event->xkey.x_root, event->xkey.y_root, &cur_x, &cur_y, &sub_window);
      event->xkey.window = win, event->xkey.x = cur_x, event->xkey.y = cur_y, event->xkey.type = KeyRelease;
    }
    return 1;
  }
}

struct ReleaseGuard {
  ReleaseGuard() { keycode = -1; button = -1; evt = 0; done = 0; next = 0; };
  Decoration* deco;
  FsmArcLink* arcs;
  XEvent* evt;
  int keycode;
  int button;
  int done;
  ReleaseGuard* next;
};

ReleaseGuard* release_list = 0;

void SetupReleaseGuard(XEvent* evt, Decoration* deco, FsmArcLink* arcs, int resent, int half)
{
  ReleaseGuard* rg = new ReleaseGuard();
  rg->deco = deco;
  rg->arcs = arcs;
  if (!half) {
    rg->done |= REL_OCCURRED;
    rg->evt = new XEvent;
    bcopy(evt, rg->evt, sizeof(XEvent)); 
    rg->evt->type = (evt->type == ButtonPress ? ButtonRelease : KeyRelease);
  }
  if (evt->type == ButtonPress)
    rg->button = evt->xbutton.button;
  else
    rg->keycode = evt->xkey.keycode;
  if (resent == RESENT_NORMAL)
    rg->done |= REL_RESEND;
  else if (resent == RESENT_SYNT)
    rg->done |= REL_RESEND_SYNT;
  rg->next = release_list;
  release_list = rg;
}

void RegisterReleaseRedir(XEvent* evt)
{
  ReleaseGuard* rg;
  int dummy;
  if (evt->type == ButtonRelease)
    for (rg = release_list; rg && rg->button != (int) evt->xbutton.button; rg = rg->next);
  else
    for (rg = release_list; rg && rg->keycode != (int) evt->xkey.keycode; rg = rg->next);
  if (rg) {
    if (rg->done & REL_RESEND_SYNT) {
      resend_multibutton(evt, 0, 1, dummy);
      rg->done &= ~REL_RESEND_SYNT;
    }
    rg->done |= (REL_OCCURRED | REL_REDIRECTED);
    rg->evt = new XEvent;
    bcopy(evt, rg->evt, sizeof(XEvent)); 
  }
}

int RegisterRelease(XEvent* evt, int& rsflag)
{
  ReleaseGuard* rg;
  Decoration* d;
  int dummy;
  if (evt->type == ButtonRelease)
    for (rg = release_list; rg && rg->button != (int) evt->xbutton.button; rg = rg->next);
  else
    for (rg = release_list; rg && rg->keycode != (int) evt->xkey.keycode; rg = rg->next);
  if (rg) {
    if (rg->done & REL_RESEND_SYNT) {
      resend_multibutton(evt, 0, 1, dummy);
      rg->done &= ~REL_RESEND_SYNT;
    }
    rg->evt = new XEvent;
    bcopy(evt, rg->evt, sizeof(XEvent)); 
    if (Fsm::ServerGrabbed() &&
        (d = Fsm::FindGrab(rg->deco, evt->type == ButtonRelease ? 0 : 1)) &&
        rg->deco != d) {
      rg->done |= (REL_OCCURRED | REL_REDIRECTED);
      return 0;
    } else {
      rg->done |= REL_OCCURRED;
      rsflag = (rg->done & REL_RESEND != 0);
      return 1;
    }
  } else 
    return 0;
}

XEvent* CheckAnyRelease()
{
  ReleaseGuard* rg;
  for (rg = release_list; rg && (!(rg->done & REL_OCCURRED) || (rg->done & REL_REDIRECTED)); rg = rg->next);
  if (rg) {
    rg->done |= REL_REDIRECTED;
    return rg->evt;
  } else 
    return 0;
}

void CatchNormalRelease(XEvent* evt)
{
  FsmArcLink* arcs;
  ReleaseGuard* rg;
  ReleaseGuard* prevrg = 0;
  Decoration* d;
  XEvent* event;
  int done, dummy;
  if (evt->type == ButtonPress)
    for (rg = release_list; rg && rg->button != (int) evt->xbutton.button; prevrg = rg, rg = rg->next);
  else
    for (rg = release_list; rg && rg->keycode != (int) evt->xkey.keycode; prevrg = rg, rg = rg->next);
  if (!rg)
    return;
  done = rg->done;
  if (!(done & REL_OCCURRED) && Fsm::ServerGrabbed() &&
      (d = Fsm::FindGrab(rg->deco, evt->type == ButtonPress ? 0 : 1)) &&
      rg->deco != d) {
    rg->done |= REL_GRABWAIT;
    return;
  }
  if (prevrg)
    prevrg->next = rg->next;
  else
    release_list = rg->next;
  if (!(done & REL_OCCURRED)) {
    event = new XEvent;
    if (evt->type == ButtonPress) {
      WaitForButtonRelease(evt->xbutton.button, rg->deco->Xwin(), event, (done & REL_RESEND ? 1 : 0), 0);
      if (done & REL_RESEND_SYNT)
        resend_multibutton(event, 0, 1, dummy);
    } else
      WaitForKeyRelease(evt->xkey.keycode, rg->deco->Xwin(), event, (done & REL_RESEND ? 1 : 0), 0);
  } else {
    event = rg->evt;
  }
  SetContext(rg->deco->Screen());
  if (!(done & REL_REDIRECTED) && (d = GetTargetWob(event)) && d->GetFsm())
    d->GetFsm()->check_second_action(arcs, rg->arcs, event, rg->done & (REL_RESEND | REL_RESEND_SYNT), fetch_event_mask(event), 1);
  else if ((arcs = rg->arcs))
    arcs->fsm->refresh_second_action(arcs, event);
  if (arcs) {
    arcs->fsm->second_action(arcs);
    delete arcs;
  }
  delete event;
  delete rg;
}

void CatchUnmapRelease(Decoration* deco)
{
  FsmArcLink* arcs;
  ReleaseGuard* rg;
  ReleaseGuard* prevrg = 0;
  Decoration* d;
  XEvent event;
  int dummy;
  for (rg = release_list; rg && ((rg->done & REL_OCCURRED) || !IsAnAncestor(deco, rg->deco)); prevrg = rg, rg = rg->next);
  if (rg && GWM_processing_frozen) {
    XAllowEvents(dpy, AsyncPointer, CurrentTime);
    XAllowEvents(dpy, AsyncKeyboard, CurrentTime);
    GWM_processing_frozen = 0;
  }
  while (rg) {
    if (prevrg)
      prevrg->next = rg->next;
    else
      release_list = rg->next;
    if (rg->button != -1) {
      WaitForButtonRelease(rg->button, rg->deco->Xwin(), &event, (rg->done & REL_RESEND ? 1 : 0), 0);
      if (rg->done & REL_RESEND_SYNT)
        resend_multibutton(&event, 0, 1, dummy);
    } else if (rg->keycode != -1)
      WaitForKeyRelease(rg->keycode, rg->deco->Xwin(), &event, (rg->done & REL_RESEND ? 1 : 0), 0);
    SetContext(rg->deco->Screen());
    if (!(rg->done & REL_REDIRECTED) && (d = GetTargetWob(&event)) && d->GetFsm())
      d->GetFsm()->check_second_action(arcs, rg->arcs, &event, rg->done & (REL_RESEND | REL_RESEND_SYNT), fetch_event_mask(&event), 1);
    else if ((arcs = rg->arcs))
      arcs->fsm->refresh_second_action(arcs, &event);
    if (arcs) {
      arcs->fsm->second_action(arcs);
      delete arcs;
    }
    delete rg;
    for (rg = (prevrg ? prevrg->next : release_list); rg && ((rg->done & REL_OCCURRED) || !IsAnAncestor(deco, rg->deco)); prevrg = rg, rg = rg->next);
  }
}

void CatchUngrabRelease()
{
  FsmArcLink* arcs;
  ReleaseGuard* rg;
  ReleaseGuard* prevrg = 0;
  Decoration* d;
  XEvent* event;
  int dummy;
  for (rg = release_list;
       rg && (!(rg->done & REL_GRABWAIT) ||
              (Fsm::ServerGrabbed() &&
               (d = Fsm::FindGrab(rg->deco, rg->button != -1 ? 0 : 1)) &&
               rg->deco != d));
       prevrg = rg, rg = rg->next);
  while (rg) {
    if (prevrg)
      prevrg->next = rg->next;
    else
      release_list = rg->next;
    if (!(rg->done & REL_OCCURRED)) {
      if (GWM_processing_frozen) {
        XAllowEvents(dpy, AsyncPointer, CurrentTime);
        XAllowEvents(dpy, AsyncKeyboard, CurrentTime);
        GWM_processing_frozen = 0;
      }
      event = new XEvent;
      if (rg->button != -1) {
        WaitForButtonRelease(rg->button, rg->deco->Xwin(), event, (rg->done & REL_RESEND ? 1 : 0), 0);
        if (rg->done & REL_RESEND_SYNT)
          resend_multibutton(event, 0, 1, dummy);
      } else if (rg->keycode != -1)
        WaitForKeyRelease(rg->keycode, rg->deco->Xwin(), event, (rg->done & REL_RESEND ? 1 : 0), 0);
    } else {
      event = rg->evt;
    }
    SetContext(rg->deco->Screen());
    if (!(rg->done & REL_REDIRECTED) && (d = GetTargetWob(event)) && d->GetFsm())
      d->GetFsm()->check_second_action(arcs, rg->arcs, event, rg->done & (REL_RESEND | REL_RESEND_SYNT), fetch_event_mask(event), 1);
    else if ((arcs = rg->arcs))
      arcs->fsm->refresh_second_action(arcs, event);
    if (arcs) {
      arcs->fsm->second_action(arcs);
      delete arcs;
    }
    delete event;
    delete rg;
    for (rg = (prevrg ? prevrg->next : release_list);
         rg && (!(rg->done & REL_GRABWAIT) ||
                (Fsm::ServerGrabbed() &&
                 (d = Fsm::FindGrab(rg->deco, rg->button != -1 ? 0 : 1)) &&
                 rg->deco != d));
         prevrg = rg, rg = rg->next);
  }
}

SCM_DEFINE(wait_for_release, "wait-for-release", 1, 1, 0,
           (SCM ev, SCM delay),
           "At a press event, wait for the corresponding release before continuing."
           "If the optional delay is given (a real number), wait at most this many seconds.")
{
  XEvent* evt;
  ReleaseGuard* rg;
  XEvent* event;
  int del = 0, dummy;
  if (!WLEVENTP(ev))
    gwm_wrong_type_arg(s_wait_for_release, 1, ev, "xevent");
  if (delay != SCM_UNDEFINED) {
    if (!gh_number_p(delay))
      gwm_wrong_type_arg(s_wait_for_release, 2, delay, "number");
    else
      del = (int) (gh_scm2double(delay)*1000.0);
  }
  evt = WL_EVENT(ev)->event();
  if (evt->type == ButtonPress) {
    for (rg = release_list; rg && rg->button != (int) evt->xbutton.button; rg = rg->next);
    if (rg && !(rg->done & REL_OCCURRED)) {
      event = new XEvent;
      if (GWM_processing_frozen ||
          !WaitForButtonRelease(evt->xbutton.button, rg->deco->Xwin(), event, (rg->done & REL_RESEND ? 1 : 0), del)) {
        delete event;
        return SCM_BOOL_F;
      }
      if (rg->done & REL_RESEND_SYNT) {
        resend_multibutton(event, 0, 1, dummy);
        rg->done &= ~REL_RESEND_SYNT;
      }
      rg->done |= (REL_OCCURRED | REL_REDIRECTED);
      rg->evt = event;
    } 
    if (rg)
      return event2scm(new WlEvent(rg->evt, 0));
  } else if (evt->type == KeyPress) {
    for (rg = release_list; rg && rg->keycode != (int) evt->xkey.keycode; rg = rg->next);
    if (rg && !(rg->done & REL_OCCURRED)) {
      event = new XEvent;
      if (GWM_processing_frozen ||
          !WaitForKeyRelease(evt->xkey.keycode, rg->deco->Xwin(), event, (rg->done & REL_RESEND ? 1 : 0), del)) {
        delete event;
        return SCM_BOOL_F;
      }
      rg->done |= (REL_OCCURRED | REL_REDIRECTED);
      rg->evt = event;
    }
    if (rg)
      return event2scm(new WlEvent(rg->evt, 0));
  }
  return SCM_BOOL_F;
}

/*
 * functions to make the different events
 */

SCM_DEFINE(make_button_event, "button", 2, 0, 0,
           (SCM button, SCM modifier),
           "Make a button event descriptor.")
{
  int but;
  must_be_number(s_make_button_event, button, 1);
  must_be_number(s_make_button_event, modifier, 2);
  but = (gh_scm2int(button) == AnyButton ? ANY : gh_scm2int(button));
  return eventpat2scm(new EventPatButton(but, gh_scm2int(modifier)));
}

SCM_DEFINE(make_buttonpress_event, "buttonpress", 2, 0, 0,
           (SCM button, SCM modifier),
           "Make a button-press event descriptor.")
{
  int but;
  must_be_number(s_make_buttonpress_event, button, 1);
  must_be_number(s_make_buttonpress_event, modifier, 2);
  but = (gh_scm2int(button) == AnyButton ? ANY : gh_scm2int(button));
  return eventpat2scm(new EventPatButtonpress(but, gh_scm2int(modifier)));
}

SCM_DEFINE(make_double_button_event, "double-button", 2, 0, 0,
           (SCM button, SCM modifier),
           "Make a double-click event descriptor.")
{
  int but;
  must_be_number(s_make_double_button_event, button, 1);
  must_be_number(s_make_double_button_event, modifier, 2);
  but = (gh_scm2int(button) == AnyButton ? ANY : gh_scm2int(button));
  return eventpat2scm(new EventPatMultibutton(but, gh_scm2int(modifier), 2));
}

SCM_DEFINE(make_triple_button_event, "triple-button", 2, 0, 0,
           (SCM button, SCM modifier),
           "Make a triple-click event descriptor.")
{
  int but;
  must_be_number(s_make_triple_button_event, button, 1);
  must_be_number(s_make_triple_button_event, modifier, 2);
  but = (gh_scm2int(button) == AnyButton ? ANY : gh_scm2int(button));
  return eventpat2scm(new EventPatMultibutton(but, gh_scm2int(modifier), 3));
}

SCM_DEFINE(make_multi_button_event, "multi-button", 3, 0, 0,
           (SCM button, SCM modifier, SCM num),
           "Make a multiple click event descriptor.")
{
  int but;
  must_be_number(s_make_multi_button_event, button, 1);
  must_be_number(s_make_multi_button_event, modifier, 2);
  must_be_number(s_make_multi_button_event, num, 3);
  but = (gh_scm2int(button) == AnyButton ? ANY : gh_scm2int(button));
  return eventpat2scm(new EventPatMultibutton(but, gh_scm2int(modifier), gh_scm2int(num)));
}

SCM_DEFINE(make_buttonrelease_event, "buttonrelease", 2, 0, 0,
           (SCM button, SCM modifier),
           "Make a button release event descriptor.")
{
  int but;
  must_be_number(s_make_buttonrelease_event, button, 1);
  must_be_number(s_make_buttonrelease_event, modifier, 2);
  but = (gh_scm2int(button) == AnyButton ? ANY : gh_scm2int(button));
  return eventpat2scm(new EventPatButtonrelease(but, gh_scm2int(modifier)));
}

void check_key_args(const char* subr, int num, SCM key, SCM mod, int &keyret, int &modret)
{
  if (mod == SCM_UNDEFINED)
    modret = (scm_is_integer(key) ? -1 : 0);
  else {
    must_be_number(subr, mod, num + 1);
    modret = gh_scm2int(mod);
  }
  if (scm_is_integer(key)) {
    keyret = gh_scm2int(key);
    if (keyret > 0) keyret = -keyret;
  } else if (gh_string_p(key)) {
    char* str = wl_getstring(key, subr);
    keyret = XStringToKeysym(str);
    delete [] str;
    if (keyret == NoSymbol)
      gwm_misc_error(subr, "unknown key name: \"~A\"\n", key);
  } else
    gwm_wrong_type_arg(subr, num, key, "string or number");
}

SCM_DEFINE(make_key_event, "key", 1, 1, 0,
           (SCM key, SCM modifier),
           "Make a key event descriptor. Takes either a key code or a key symbol.")
{
  int keysym;
  int mod;
  check_key_args(s_make_key_event, 1, key, modifier, keysym, mod);
  return eventpat2scm(new EventPatKey(keysym, mod));
}

SCM_DEFINE(make_keypress_event, "keypress", 1, 1, 0,
           (SCM key, SCM modifier),
           "Make a key press event descriptor. Takes either a key code or a key symbol.")
{
  int keysym;
  int mod;
  check_key_args(s_make_keypress_event, 1, key, modifier, keysym, mod);
  return eventpat2scm(new EventPatKeypress(keysym, mod));
}

SCM_DEFINE(make_keyrelease_event, "keyrelease", 1, 1, 0,
           (SCM key, SCM modifier),
           "Make a key release event descriptor. Takes either a key code or a key symbol.")
{
  int keysym;
  int mod;
  check_key_args(s_make_keyrelease_event, 1, key, modifier, keysym, mod);
  return eventpat2scm(new EventPatKeyrelease(keysym, mod));
}

SCM_DEFINE(make_user_event, "user-event", 1, 0, 0,
           (SCM tag),
           "Make a user event descriptor, identified by tag.")
{
  if (!gh_symbol_p(tag))
    gwm_wrong_type_arg(s_make_user_event, 1, tag, "symbol");
  return eventpat2scm(new EventPatUser(tag));
}

SCM_DEFINE(make_propertychange_event, "property-change", 1, 0, 0,
           (SCM name),
           "Make a property-change event descriptor, for the property name (a symbol).")
{
  Atom property;

  if (gh_symbol_p(name)) {
    char* str = wl_getstring(scm_symbol_to_string(name), s_make_propertychange_event);
    property = XInternAtom(dpy, str, False);
    delete [] str;
  } else if (name == SCM_BOOL_T)
    property = (Atom) ANY;
  else
    gwm_wrong_type_arg(s_make_propertychange_event, 1, name, "symbol");
  return eventpat2scm(new EventPatPropertychange(property));
}

SCM_DEFINE(make_clientmessage_event, "client-message", 1, 0, 0,
           (SCM name),
           "Make a client-message event descriptor, for the message name (a symbol).")
{
  Atom property;
  char* str;

  if (!gh_symbol_p(name))
    gwm_wrong_type_arg(s_make_clientmessage_event, 1, name, "symbol");
  str = wl_getstring(scm_symbol_to_string(name), s_make_clientmessage_event);
  property = XInternAtom(dpy, str, False);
  delete [] str;
  return eventpat2scm(new EventPatClientmessage(property));
}

SCM_DEFINE(make_movement_event, "movement", 0, 0, 0,
           (),
           "Make a movement event descriptor.")
{
  return v_movement;
}

SCM_DEFINE(make_enter_event, "enter", 0, 0, 0,
           (),
           "Make a enter deco event descriptor.")
{
  return v_enter;
}

SCM_DEFINE(make_leave_event, "leave", 0, 0, 0,
           (),
           "Make a leave deco event descriptor.")
{
  return v_leave;
}

SCM_DEFINE(make_focus_in_event, "focus-in", 0, 0, 0,
           (),
           "Make a deco focus-in event descriptor.")
{
  return v_focus_in;
}

SCM_DEFINE(make_focus_out_event, "focus-out", 0, 0, 0,
           (),
           "Make a deco focus-out event descriptor.")
{
  return v_focus_out;
}

SCM_DEFINE(make_map_event, "map-event", 0, 0, 0,
           (),
           "Make a map deco event descriptor.")
{
  return v_map_event;
}

SCM_DEFINE(make_unmap_event, "unmap-event", 0, 0, 0,
           (),
           "Make a unmap deco event descriptor.")
{
  return v_unmap_event;
}

SCM_DEFINE(make_resize_event, "resize-event", 0, 0, 0,
           (),
           "Make a resize deco event descriptor.")
{
  return v_resize_event;
}

SCM_DEFINE(make_move_event, "move-event", 0, 0, 0,
           (),
           "Make a move deco event descriptor.")
{
  return v_move_event;
}

SCM_DEFINE(make_stack_event, "stack-event", 0, 0, 0,
           (),
           "Make a deco stacking change event descriptor.")
{
  return v_stack_event;
}

SCM_DEFINE(make_name_change_event, "name-change", 0, 0, 0,
           (),
           "Make a name-change event descriptor.")
{
  return v_name_change;
}

SCM_DEFINE(make_window_icon_pixmap_change_event, "icon-pixmap-change", 0, 0, 0,
           (),
           "Make a icon-pixmap-change event descriptor.")
{
  return v_window_icon_pixmap_change;
}

SCM_DEFINE(make_opening_event, "opening", 0, 0, 0,
           (),
           "Make a deco opening event descriptor.")
{
  return v_opening_event;
}

SCM_DEFINE(make_closing_event, "closing", 0, 0, 0,
           (),
           "Make a deco closing event descriptor.")
{
  return v_closing_event;
}


    SCM_GLOBAL_VARIABLE_INIT(v_any, "any", gh_long2scm(ANY));
    SCM_GLOBAL_VARIABLE_INIT(v_alone, "alone", gh_long2scm(0));
    SCM_GLOBAL_VARIABLE_INIT(v_button_1_mask, "button-1-mask", gh_long2scm(Button1Mask));
    SCM_GLOBAL_VARIABLE_INIT(v_button_2_mask, "button-2-mask", gh_long2scm(Button2Mask));
    SCM_GLOBAL_VARIABLE_INIT(v_button_3_mask, "button-3-mask", gh_long2scm(Button3Mask));
    SCM_GLOBAL_VARIABLE_INIT(v_button_4_mask, "button-4-mask", gh_long2scm(Button4Mask));
    SCM_GLOBAL_VARIABLE_INIT(v_button_5_mask, "button-5-mask", gh_long2scm(Button5Mask));
    SCM_GLOBAL_VARIABLE_INIT(v_shift_mask, "shift-mask", gh_long2scm(ShiftMask));
    SCM_GLOBAL_VARIABLE_INIT(v_lock_mask, "lock-mask", gh_long2scm(LockMask));
    SCM_GLOBAL_VARIABLE_INIT(v_control_mask, "control-mask", gh_long2scm(ControlMask));
    SCM_GLOBAL_VARIABLE_INIT(v_alt_mask, "alt-mask", gh_long2scm(Mod1Mask));
    SCM_GLOBAL_VARIABLE_INIT(v_modifier_1_mask, "modifier-1-mask", gh_long2scm(Mod1Mask));
    SCM_GLOBAL_VARIABLE_INIT(v_modifier_2_mask, "modifier-2-mask", gh_long2scm(Mod2Mask));
    SCM_GLOBAL_VARIABLE_INIT(v_modifier_3_mask, "modifier-3-mask", gh_long2scm(Mod3Mask));
    SCM_GLOBAL_VARIABLE_INIT(v_modifier_4_mask, "modifier-4-mask", gh_long2scm(Mod4Mask));
    SCM_GLOBAL_VARIABLE_INIT(v_modifier_5_mask, "modifier-5-mask", gh_long2scm(Mod5Mask));

/*
 * returns the coords of the click relative to the client as a list
 * (logical-x logical-y x y) for the current window's client
 */

SCM_DEFINE(event_logical_coords, "event-window-coords", 2, 0, 0,
           (SCM ev, SCM window),
           "Get logical coordinates in window of event.")
{
  ClientWindow* cw;
  Decoration* deco;
  int x=0, y=0;
  Window child;
  SCM list[6];
  XEvent* evt;
  XSizeHints *normal_hints;

  if (!WLEVENTP(ev))
    gwm_wrong_type_arg(s_event_logical_coords, 1, ev, "xevent");
  must_be_valid_window(s_event_logical_coords, window, 2);
  evt = WL_EVENT(ev)->event();
  deco = WL_DECO(window);
  cw = WL_WINDOW(window);
  normal_hints = cw->NormalHints();
  if ((deco->Type() & WindowStatus) && cw->Client() && !cw->PendingClose())
    XTranslateCoordinates(dpy, evt->xbutton.root,
                          cw->InnerWin(), evt->xbutton.x_root,
                          evt->xbutton.y_root, &x, &y, &child);
  list[0] = gh_int2scm(
      (x - (normal_hints->flags & (PBaseSize|PMinSize) ?
            normal_hints->base_width : 0))
       / (normal_hints->flags & PResizeInc ?
          normal_hints->width_inc : 1));
  list[1] = gh_int2scm(
      (y - (normal_hints->flags & (PBaseSize|PMinSize) ?
            normal_hints->base_height : 0))
       / (normal_hints->flags & PResizeInc ?
          normal_hints->height_inc : 1));
  list[2] = gh_int2scm(x);
  list[3] = gh_int2scm(y);

  XTranslateCoordinates(dpy, evt->xbutton.root, deco->Top()->Xwin(),
                        evt->xbutton.x_root, evt->xbutton.y_root, &x, &y, &child);
  list[4] = gh_int2scm(x);
  list[5] = gh_int2scm(y);

  return gh_list(list[0], list[1], list[2],
                 list[3], list[4], list[5], SCM_UNDEFINED);
}

SCM_DEFINE(send_user_event, "send-user-event", 2, 0, 1,
           (SCM tag, SCM deco, SCM args),
           "Send a user event to a decoration. The tag can be either a symbol identifying" 
           "the user event, or a list beginning with such a tag. The user event is"
           "sent to all parts of deco as well, unless the key :propagate is set to false.")
{
  Decoration* wob;
  XEvent evt;
  int propagate;
  int n, cn;
  SCM ctx, sub;
  n = wl_separate_context(args, sub, ctx, cn, s_send_user_event);
  if (n>0)
    gwm_wrong_num_args(s_send_user_event, n+2);
  if (!gh_symbol_p(tag) && !(gh_pair_p(tag) && gh_symbol_p(SCM_CAR(tag))))
    gwm_wrong_type_arg(s_send_user_event , 1, tag, "symbol or list beginning with symbol");
  must_be_valid_deco(s_send_user_event, deco, 2);
  wob = WL_DECO(deco);
  propagate = (gwm_get_keyword(k_propagate, ctx, cn, SCM_BOOL_T) != SCM_BOOL_F);
  evt.xany.type = GWMUserEvent;		    /* our event */
  evt.xany.send_event = propagate;                /* propagate further */
  evt.xany.display = (Display *) SCM2PTR(tag);    /* the message */
  evt.xany.window = (Window) 0;		    /* used to be the sender */
  wob->EventHandler(&evt);
  return SCM_UNSPECIFIED;
}

//*******************************************//

TimerEvent* timer_queue = 0;
TimerEvent* timer_queue_last = 0;
GwmEvent* gwmevent_queue = 0;
GwmEvent* gwmevent_queue_last = 0;
GwmEvent* gwmevent_old_stack = 0;
GwmEvent* gwmevent_current = 0;

void EnqueueTimerEvent(XEvent* ev, class Decoration* w, int t)
{
  TimerEvent* te;
  TimerEvent* tp;
  te = new TimerEvent(ev, w, t);
  if (!timer_queue) {
    timer_queue = timer_queue_last = te;
  } else if (t ? timer_queue_last->time - t <= 0 : timer_queue_last->time == 0) {
    timer_queue_last->next = te;
    timer_queue_last = te;
  } else if (t ? timer_queue->time - t > 0 : timer_queue->time) {
    te->next = timer_queue;
    timer_queue = te;
  } else {
    tp = timer_queue;
    if (t)
      while (tp->next && tp->next->time - t <= 0)
        tp = tp->next;
    else
      while (tp->next && tp->next->time == 0)
        tp = tp->next;
    te->next = tp->next;
    tp->next = te;
  }
}

void EnqueueGwmEvent(XEvent* evt, class Decoration* deco)
{
  GwmEvent* ge;
  GwmEvent* e = gwmevent_queue;
  while (e) {
    if (e->wob == deco && e->event->type == evt->type) {
      delete evt;
      return;
    }
    e = e->next;
  }
  e = gwmevent_current;
  while (e) {
    if (e->wob == deco && e->event->type == evt->type) {
      delete evt;
      return;
    }
    e = e->cause;
  }
  ge = new GwmEvent(evt, deco, gwmevent_current);
  if (!gwmevent_queue) {
    gwmevent_queue = gwmevent_queue_last = ge;
  } else {
    gwmevent_queue_last->next = ge;
    gwmevent_queue_last = ge;
  }
}

int NextTimerDelay()
{
  int del;
  if (gwmevent_queue)
    return 0;
  else if (timer_queue) {
    if (timer_queue->time == 0)
      return 0;
    del = timer_queue->time - gwm_proper_time();
    return (del > 0 ? del : 0);
  } else
    return -1;
}

void ProcessGwmEvents()
{
  GwmEvent* ge;
  GwmEvent* ge2;
  GwmEvent* old = gwmevent_current;
  while (gwmevent_queue) {
    ge = gwmevent_current = gwmevent_queue;
    gwmevent_queue = ge->next;
    ge->next = gwmevent_old_stack;
    gwmevent_old_stack = ge;
    if (ge->wob->Valid() > 0)
      ge->wob->EventHandler(ge->event);
  }
  gwmevent_current = old;
  ge2 = 0;
  ge = gwmevent_old_stack;
  while (ge) {
    if (ge->count == 0) {
      if (ge2) {
        ge2->next = ge->next;
        delete ge;
        ge = ge2->next;
      } else { 
        gwmevent_old_stack = ge->next;
        delete ge;
        ge = gwmevent_old_stack;
      }
    } else {
      ge2 = ge;
      ge = ge->next;
    }
  }
}

void ProcessTimerEvents()
{
  TimerEvent *te, *last, *tmp;
  int time;
  ProcessGwmEvents();
  if (!timer_queue)
    return;
  time = gwm_proper_time();
  for (tmp=timer_queue, last=0; tmp && tmp->time == 0; last=tmp, tmp=tmp->next);
  for (; tmp && tmp->time - time <= 0; last=tmp, tmp=tmp->next);
  if (!last)
    return;
  te = timer_queue;
  while (te) {
    timer_queue = te->next;
    if (!timer_queue)
      timer_queue_last = 0;
    if (te->event->type == GWMUserEvent && gh_pair_p(PTR2SCM(te->event->xany.display)))
      scm_gc_unprotect_object(PTR2SCM(te->event->xany.display));
    if (te->wob->Valid() > 0)
      te->wob->EventHandler(te->event);
    delete te;
    if (te == last)
      te = 0;
    else
      te = timer_queue;
  }
}

SCM_DEFINE(send_timer_event, "send-timer-event", 3, 0, 1,
           (SCM tag, SCM deco, SCM del, SCM args),
           "Send a delayed user event to a decoration, after del seconds (a real number)."
           "The tag can be either a symbol identifying the user event, or a list beginning"
           "with such a tag. Unlike for 'send-user-event', the event is not sent to the"
           "parts of the deco, unless the key :propagate is set to true.")
{
  Decoration* wob;
  XEvent* evt;
  int propagate;
  int time;
  int n, cn;
  SCM ctx, sub;
  n = wl_separate_context(args, sub, ctx, cn, s_send_timer_event);
  if (n>0)
    gwm_wrong_num_args(s_send_timer_event, n+3);
  if (!gh_symbol_p(tag) && !(gh_pair_p(tag) && gh_symbol_p(SCM_CAR(tag))))
    gwm_wrong_type_arg(s_send_timer_event , 1, tag, "symbol or list beginning with symbol");
  must_be_valid_deco(s_send_timer_event, deco, 2);
  if (!gh_number_p(del))
    gwm_wrong_type_arg(s_send_timer_event, 3, del, "number");
  time = gwm_proper_time() + (int) (gh_scm2double(del)*1000.0);
  wob = WL_DECO(deco);
  if (gh_pair_p(tag)) {
    tag = scm_cons(SCM_CAR(tag), SCM_CDR(tag));
    scm_gc_protect_object(tag);    // protect from GC during time in queue
  }
  propagate = (gwm_get_keyword(k_propagate, ctx, cn, SCM_BOOL_F) != SCM_BOOL_F);
  evt = new XEvent;
  evt->xany.type = GWMUserEvent;		 /* our event */
  evt->xany.send_event = propagate;              /* propagate further */
  evt->xany.display = (Display *) SCM2PTR(tag);  /* the message */
  evt->xany.window = (Window) 0;		 /* used to be the sender */
  EnqueueTimerEvent(evt, wob, (time ? time : 1));
  return SCM_UNSPECIFIED;
}

/* send  a client message event */
SCM_DEFINE(send_client_message, "send-client-message", 2, 0, 1,
           (SCM window, SCM name, SCM args),
           "Send a client message event with name (a symbol) to window. The following"
           "args can be either a string, some numbers, or some symbols.")
{
    Atom      property_name;
    unsigned long nitems;
    unsigned char *buffer;
    int i, argc;
    XClientMessageEvent CliMessage;
    Window hook;
    long mask;
    char* str;

    argc = scm_ilength(args);
    if (argc < 1)
	gwm_wrong_num_args(s_send_client_message, argc+2);

    if (!WLSCRWINP(window))
      gwm_wrong_type_arg(s_send_client_message, 1, window, "window or screen");
    if (!gh_symbol_p(name))
      gwm_wrong_type_arg(s_send_client_message, 2, name, "symbol");
    str = wl_getstring(scm_symbol_to_string(name), s_send_client_message);
    property_name = XInternAtom(dpy, str, False);
    delete [] str;
    if (WLWINDOWP(window)) {
      hook = (WL_WINDOW(window)->PendingClose() ? 0 : WL_WINDOW(window)->InnerWin());
      mask = 0;
    } else {
      hook = WL_SCREEN(window)->Root();
      mask = SubstructureNotifyMask;
    }
    CliMessage.message_type = property_name;
    CliMessage.window = hook;
    CliMessage.type = ClientMessage;

    if (gh_string_p(SCM_CAR(args)) && (argc == 1)) {
	CliMessage.format = 8;
        buffer = (unsigned char *) scm_i_string_chars(SCM_CAR(args));
	nitems = scm_i_string_length(SCM_CAR(args));
	if (nitems > 20)
	    nitems = 20;
	bcopy(buffer, CliMessage.data.b, nitems);
    }
    else if (scm_is_integer(SCM_CAR(args)) && (argc < 5)) {
	CliMessage.format = 32;
	for (i = 0; i < argc; i++) {
            if (!scm_is_integer(SCM_CAR(args)))
              gwm_wrong_type_arg(s_send_client_message, i+3,
                                  SCM_CAR(args), "number");
	    CliMessage.data.l[i] = (long) (gh_scm2int(SCM_CAR(args)));
            args = SCM_CDR(args);
	}
    }
    else if (scm_is_integer(SCM_CAR(args)) && (argc < 10)) {
	CliMessage.format = 16;
	for (i = 0; i < argc; i++) {
            if (!scm_is_integer(SCM_CAR(args)))
              gwm_wrong_type_arg(s_send_client_message, i+3,
                                  SCM_CAR(args), "number");
	    CliMessage.data.s[i] = (short) (gh_scm2int(SCM_CAR(args)));
            args = SCM_CDR(args);
	}
    }
    else if (gh_symbol_p(SCM_CAR(args)) && (argc < 5)) {
        char* str;
	CliMessage.format = 32;
	for (i = 0; i < argc; i++) {
            if (!gh_symbol_p(SCM_CAR(args)))
              gwm_wrong_type_arg(s_send_client_message, i+3,
                                  SCM_CAR(args), "symbol");
            str = wl_getstring(scm_symbol_to_string(SCM_CAR(args)), s_send_client_message);
	    CliMessage.data.l[i] = (long) XInternAtom(dpy, str, False);
            args = SCM_CDR(args);
            delete [] str;
	}
    }
    else
	return SCM_BOOL_F;

    if (hook && XSendEvent(dpy, hook, False, mask, (XEvent *) &CliMessage))
      return SCM_BOOL_T;
    else
      return SCM_BOOL_F;
}

void send_gwm_event(Decoration* deco, int type)
{
    XEvent* evt;
    evt = new XEvent;
    evt->xany.type = type;
    evt->xany.display = 0;
    evt->xany.window = (Window) 0;
    EnqueueGwmEvent(evt, deco);
}

void send_gwm_crossing_event(class Decoration* deco, XCrossingEvent* ev, int type)
{
    XEvent* evt;
    evt = new XEvent;
    evt->xcrossing.type = type;
    evt->xcrossing.display = ev->display;
    evt->xcrossing.window = 0;
    evt->xcrossing.root = 0;
    evt->xcrossing.time = ev->time;
    evt->xcrossing.x = ev->x;
    evt->xcrossing.y = ev->y;
    evt->xcrossing.x_root = ev->x_root;
    evt->xcrossing.y_root = ev->y_root;
    EnqueueTimerEvent(evt, deco, 0);
}

void fill_x_key_event(XKeyPressedEvent* evt, int keycode, int modifier, Window win, Window root)
{
    evt->type = KeyPress;
    evt->display = dpy;
    evt->window = evt->subwindow = win;
    evt->root = root;
    evt->time = CurrentTime;
    evt->x = evt->y = evt->x_root = evt->y_root = 0;
    evt->same_screen = 1;
    evt->keycode = keycode;
    evt->state = modifier;
}

SCM_DEFINE(send_key_to_client, "send-key-to-window", 2, 1, 0,
           (SCM window, SCM key, SCM modifier),
           "Send a synthetic key event to the window. Key can be either a key code"
           "or a key symbol.")
{
  Window w, root;
  XEvent event;
  int sym, code;
  int mod;
  must_be_window(s_send_key_to_client, window, 1);
  check_key_args(s_send_key_to_client, 2, key, modifier, sym, mod);
  if (sym == -1)
    gwm_misc_error(s_send_key_to_client, "no valid key specification\n", 0);
  if (mod == -1)
    mod = 0;
  if (sym < 0)
    code = -sym;
  else {
    code = XKeysymToKeycode(dpy, sym);
    mod |= keysym_to_keycode_modifier_mask(sym, code);
  }
  if (!WL_WINDOW(window)->PendingClose()) {
    w = WL_WINDOW(window)->InnerWin();
    root = WL_SCREEN(window)->Root();
    fill_x_key_event((XKeyPressedEvent*) &event, code, mod, w, root);
    XSendEvent(dpy, w, False, KeyPressMask, &event);
    event.type = KeyRelease;
    XSendEvent(dpy, w, False, KeyReleaseMask, &event);
  }
  return SCM_UNSPECIFIED;
}

SCM_DEFINE(send_string_to_client, "send-string-to-window", 2, 1, 0,
           (SCM window, SCM string, SCM modifier),
           "Send synthetic key events to the window, transfering the characters"
           "of the string.")
{
  Window w, root;
  XEvent event;
  unsigned char *p = 0;
  unsigned int keysym;
  int mod, globalmod, keycode;
  int i, len;

  must_be_window(s_send_string_to_client, window, 1);
  must_be_string(s_send_string_to_client, string, 2);
  if (!WL_WINDOW(window)->PendingClose()) {
    w = WL_WINDOW(window)->InnerWin();
    root = WL_SCREEN(window)->Root();
    p = (unsigned char*) scm_i_string_chars(string);
    len =  scm_i_string_length(string);
    if (modifier != SCM_UNDEFINED) {
      must_be_number(s_send_string_to_client, modifier, 3);
      globalmod = gh_scm2int(modifier);
    } else
      globalmod = 0;
    for (i=0; i<len; i++, p++) {				/* we MUST re-init event each time */
      keysym = *p;
      if (!(keycode = XKeysymToKeycode(dpy, keysym))) {
        gwm_warning("No key binding for character ~S\n", gh_char2scm(keysym));
        continue;
      }
      mod = keysym_to_keycode_modifier_mask(keysym, keycode) | globalmod;
      fill_x_key_event((XKeyPressedEvent*) &event, keycode, mod, w, root);
      XSendEvent(dpy, w, False, KeyPressMask, &event);
      event.type = KeyRelease;
      XSendEvent(dpy, w, False, KeyReleaseMask, &event);
    }
  }
  return SCM_UNSPECIFIED;
}

/* same with button: button modifier x y
 */
void fill_x_button_event(XButtonEvent* evt, int button, int modifier, int x, int y, int x_root, int y_root, Window child, Window root)
{
  evt->type = ButtonPress;
  evt->display = dpy;
  evt->window = child;
  evt->subwindow = 0;
  evt->root = root;
  evt->time = CurrentTime;
  evt->x = x;
  evt->y = y;
  evt->x_root = x_root;
  evt->y_root = y_root;
  evt->same_screen = 1;
  evt->button = button;
  evt->state = modifier;
}

/* look which window would receive button event
 */
Window WindowGettingButtonEvent(Window w, int x, int y)
{
  int x2, y2;
  Window child, w1 = w, w2 = w;
  Window d1, *d3, parent;
  unsigned int d4;
  XWindowAttributes wa;

  while (1) {
    XTranslateCoordinates(dpy, w1, w2, x, y, &x2, &y2, &child);
    x = x2, y = y2, w1 = w2;
    if (!child) break;
    w2 = child;
  }
  while (1) {
    XGetWindowAttributes(dpy, w1, &wa);
    if (w1 == w || wa.all_event_masks & (ButtonPressMask | ButtonReleaseMask))
      break;
    XQueryTree(dpy, w1, &d1, &parent, &d3, &d4);
    if (d3) XFree(d3);
    if (!parent) break;
    w1 = parent;
  }
  if (!(wa.all_event_masks & (ButtonPressMask | ButtonReleaseMask)))
    return 0;
  return w1;
}

SCM_DEFINE(send_button_to_client, "send-button-to-window", 5, 0, 0,
           (SCM window, SCM button, SCM modifier, SCM x, SCM y),
           "Send synthetic button events to the window, for pressing and releasing"
           "the button at position (x, y) relative the window.")
{
  Window w, root;
  XEvent event;
  int but, mod, x1, y1, x2, y2, x_root, y_root;
  Window child, dummy;

  must_be_window(s_send_button_to_client, window, 1);
  must_be_number(s_send_button_to_client, button, 2);
  must_be_number(s_send_button_to_client, modifier, 3);
  must_be_number(s_send_button_to_client, x, 4);
  must_be_number(s_send_button_to_client, y, 5);

  if (!WL_WINDOW(window)->PendingClose()) {
    w = WL_WINDOW(window)->InnerWin();
    root = WL_SCREEN(window)->Root();
    but = gh_scm2int(button);
    mod = gh_scm2int(modifier);
    x1 = gh_scm2int(x);
    y1 = gh_scm2int(y);
    XTranslateCoordinates(dpy, w, root, x1, y1, &x_root, &y_root, &child);
    child = WindowGettingButtonEvent(w, x1, y1);
    if (!child)
      return SCM_UNSPECIFIED;
    x2 = x1, y2 = y1;
    XTranslateCoordinates(dpy, w, child, x2, y2, &x1, &y1, &dummy);
    fill_x_button_event((XButtonEvent*) &event, but, mod, x1, y1, x_root, y_root, child, root);
    XSendEvent(dpy, child, False, ButtonPressMask, &event);
    event.type = ButtonRelease;
    XSendEvent(dpy, child, False, ButtonReleaseMask, &event);
  }
  return SCM_UNSPECIFIED;
}

/*
 * recuperates parameters of the triggering event
 */

SCM_DEFINE(get_event_deco, "event-deco", 1, 0, 0,
           (SCM ev),
           "Return the deco that got this event.")
{
  Decoration* wob;
  if (!WLEVENTP(ev))
    gwm_wrong_type_arg(s_get_event_deco, 1, ev, "xevent");
  wob = LookUpDeco(WL_EVENT(ev)->event()->xany.window);
  if (wob)
    return wob->scm();
  else
    return SCM_BOOL_F;
}

SCM_DEFINE(get_event_data, "event-data", 1, 0, 0,
           (SCM ev),
           "Get data from a user event or a client message event.")
{
  SCM res;
  XClientMessageEvent* clev;
  if (!WLEVENTP(ev))
    gwm_wrong_type_arg(s_get_event_data, 1, ev, "xevent");
  if (WL_EVENT(ev)->event()->type == ClientMessage) {
    clev = (XClientMessageEvent*) WL_EVENT(ev)->event();
    switch (clev->format) {
    case 8:
      return (scm_makfrom0str(clev->data.b));
    case 16:
      {
        int i;
        res = SCM_EOL;
        for (i=9;i>=0;i--) {
          res = gh_cons(gh_int2scm(clev->data.s[i]), res);
        }
        return res;
      }
    case 32:
      {
        int i;
        res = SCM_EOL;
        for (i=4;i>=0;i--) {
          res = gh_cons(gh_int2scm(clev->data.l[i]), res);
        }
        return res;
      }
    }
  } else if (WL_EVENT(ev)->event()->type == GWMUserEvent) {
    return (SCM) PTR2SCM(WL_EVENT(ev)->event()->xany.display);
  } else
    return SCM_BOOL_F;
}

SCM_DEFINE(get_event_state, "event-modifier", 1, 0, 0,
           (SCM ev),
           "Get modifiers from key, button or movement event.")
{
  if (!WLEVENTP(ev))
    gwm_wrong_type_arg(s_get_event_state, 1, ev, "xevent");
  if (WL_EVENT(ev)->has_state())
    return gh_int2scm(WL_EVENT(ev)->state());
  else
    return SCM_BOOL_F;
}

SCM_DEFINE(get_event_code, "event-code", 1, 0, 0,
           (SCM ev),
           "Get key or button code from event.")
{
  XEvent* evt;
  if (!WLEVENTP(ev))
    gwm_wrong_type_arg(s_get_event_code, 1, ev, "xevent");
  evt = WL_EVENT(ev)->event();
  if (evt->type == ButtonPress || evt->type == ButtonRelease)
    return gh_int2scm(evt->xbutton.button);
  else if (evt->type == KeyPress || evt->type == KeyRelease)
    return gh_int2scm(evt->xkey.keycode);
  else
    return SCM_BOOL_F;
}

SCM_DEFINE(get_event_x, "event-x", 1, 0, 0,
           (SCM ev),
           "Get x position of event (relative the root).")
{
  if (!WLEVENTP(ev))
    gwm_wrong_type_arg(s_get_event_x, 1, ev, "xevent");
  if (WL_EVENT(ev)->has_pos())
    return gh_int2scm(WL_EVENT(ev)->rootx());
  else
    return SCM_BOOL_F;
}

SCM_DEFINE(get_event_y, "event-y", 1, 0, 0,
           (SCM ev),
           "Get y position of event (relative the root).")
{
  if (!WLEVENTP(ev))
    gwm_wrong_type_arg(s_get_event_y, 1, ev, "xevent");
  if (WL_EVENT(ev)->has_pos())
    return gh_int2scm(WL_EVENT(ev)->rooty());
  else
    return SCM_BOOL_F;
}

SCM_DEFINE(get_event_x_relative, "event-relative-x", 1, 0, 0,
           (SCM ev),
           "Get x position of event relative to its deco.")
{
  if (!WLEVENTP(ev))
    gwm_wrong_type_arg(s_get_event_x_relative, 1, ev, "xevent");
  if (WL_EVENT(ev)->has_pos())
    return gh_int2scm(WL_EVENT(ev)->relx());
  else
    return SCM_BOOL_F;
}

SCM_DEFINE(get_event_y_relative, "event-relative-y", 1, 0, 0,
           (SCM ev),
           "Get y position of event relative to its deco.")
{
  if (!WLEVENTP(ev))
    gwm_wrong_type_arg(s_get_event_y_relative, 1, ev, "xevent");
  if (WL_EVENT(ev)->has_pos())
    return gh_int2scm(WL_EVENT(ev)->rely());
  else
    return SCM_BOOL_F;
}

SCM_DEFINE(get_event_time, "event-time", 1, 0, 0,
           (SCM ev),
           "Get event time.")
{
  if (!WLEVENTP(ev))
    gwm_wrong_type_arg(s_get_event_time, 1, ev, "xevent");
  return gh_int2scm(fetch_event_time(WL_EVENT(ev)->event()));
}

SCM_DEFINE(get_event_key, "event-key", 1, 0, 0,
           (SCM ev),
           "Get key symbol from a key event.")
{
  char s[81];
  int l;
  XEvent* evt;
  if (!WLEVENTP(ev))
    gwm_wrong_type_arg(s_get_event_key, 1, ev, "xevent");
  evt = WL_EVENT(ev)->event();
  if ((evt->type == KeyPress || evt->type == KeyRelease)
      && (l = XLookupString(&evt->xkey, s, 80, 0, 0))) {
    s[l] = '\0';
    return scm_makfrom0str(s);
  } else
    return SCM_BOOL_F;
}

SCM_DEFINE(get_event_type, "event-type", 1, 0, 0,
           (SCM ev),
           "Get type of event or event description as a symbol.")
{
  int i;
  if (WLEVENTP(ev)) {
    i = fetch_event_index(WL_EVENT(ev)->event());
    if (i<0)
      return SCM_BOOL_F;
    else
      return gh_symbol2scm(event_name[i]);
  } else if (WLEVENTPATP(ev)) {
    i = WL_EVENTPAT(ev)->Index();
    return gh_symbol2scm(event_name[i]);
  } else
    gwm_wrong_type_arg(s_get_event_type, 1, ev, "xevent or event");
}

void init_scm_event()
{
  scm_tc16_wlevent = scm_make_smob_type("xevent", 0);
  scm_set_smob_mark(scm_tc16_wlevent, mark_wlevent);
  scm_set_smob_free(scm_tc16_wlevent, free_wlevent);
  scm_set_smob_print(scm_tc16_wlevent, print_wlevent);
  scm_tc16_eventpat = scm_make_smob_type("event", 0);
  scm_set_smob_mark(scm_tc16_eventpat, mark_eventpat);
  scm_set_smob_free(scm_tc16_eventpat, free_eventpat);
  scm_set_smob_print(scm_tc16_eventpat, print_eventpat);
  scm_set_smob_equalp(scm_tc16_eventpat, equal_eventpat);

  v_movement = eventpat2scm(new EventPatMovement());
  scm_gc_protect_object(v_movement);
  v_enter = eventpat2scm(new EventPatEnter());
  scm_gc_protect_object(v_enter);
  v_leave = eventpat2scm(new EventPatLeave());
  scm_gc_protect_object(v_leave);
  v_focus_in = eventpat2scm(new EventPatFocusin());
  scm_gc_protect_object(v_focus_in);
  v_focus_out = eventpat2scm(new EventPatFocusout());
  scm_gc_protect_object(v_focus_out);
  v_map_event = eventpat2scm(new EventPatMap());
  scm_gc_protect_object(v_map_event);
  v_unmap_event = eventpat2scm(new EventPatUnmap());
  scm_gc_protect_object(v_unmap_event);
  v_resize_event = eventpat2scm(new EventPatResize());
  scm_gc_protect_object(v_resize_event);
  v_move_event = eventpat2scm(new EventPatMove());
  scm_gc_protect_object(v_move_event);
  v_stack_event = eventpat2scm(new EventPatStack());
  scm_gc_protect_object(v_stack_event);
  v_name_change = eventpat2scm(new EventPatPropertychange(XA_WM_NAME));
  scm_gc_protect_object(v_name_change);
  v_window_icon_pixmap_change = eventpat2scm(new EventPatIconchange());
  scm_gc_protect_object(v_window_icon_pixmap_change);
  v_opening_event = eventpat2scm(new EventPatOpening());
  scm_gc_protect_object(v_opening_event);
  v_closing_event = eventpat2scm(new EventPatClosing());
  scm_gc_protect_object(v_closing_event);

#include "event.x"
}
