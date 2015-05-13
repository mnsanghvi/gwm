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
 * ClientWindow Object *
 * 		       *
 \*********************/

#include <stdio.h>

#include <guile/gh.h>

#include "gwm.hh"
#include "gwmfunc.hh"
#include "error.hh"
#include "deco.hh"
#include "event.hh"
#include "fsm.hh"
#include "paint.hh"
#include "cursor.hh"
#include "screen.hh"
#include "client.hh"
#include "drawing.hh"

/*
struct GroupHead {
  Window leader;
  SCM group;
  GroupHead* next;
};

#define GROUPTABLELEN 53

struct CachedProperties { 	// common to window & icon
  SCM clientclass;		
  SCM clientname;
  SCM machinename;
  SCM windowname;
  SCM iconname;
  XWMHints wm_hints;
  XWMHints old_wm_hints;
  XSizeHints normal_hints;
  unsigned int wm_state;	// last updated state 
  Window transient_for;	// raw X11 hint 
  Window client_group;	// the logical GWM group
  int colormap_windows_size;	// list of sub-windows that must 
  Window* colormap_windows;	// have colormap installed 
  int colormap_windows_index;	// current selected one 
      				// standard ICCC protocols 
  unsigned int wm_take_focus : 1;
  unsigned int wm_save_yourself : 1;
  unsigned int wm_delete_window : 1;
  unsigned int new_normal_hints : 1;
};

class ClientWindow {
  friend void ScreenContext::RegisterWindow(ClientWindow* win);
  friend void ScreenContext::UnregisterWindow(ClientWindow* win);
public:
  ClientWindow(Window win, ScreenContext* scr, XWindowAttributes* wa);
  ClientWindow(IMenu* mn, ScreenContext* scr, XWindowAttributes* wa);
  ~ClientWindow();
  class Decoration* MakeIconWindow();
  void UpdateCachedProperties(Window window, ScreenContext* scr);
  void FreeCachedProperties();
  void UpdateCachedProperty(Atom property_atom, ScreenContext* scr);
  void Update_XA_WM_NAME(Window win);
  void Update_XA_WM_ICON_NAME(Window win);
  void Update_XA_WM_HINTS(Window win, int backup);
  void Update_XA_WM_NORMAL_HINTS(Window win);
  void Update_XA_WM_TRANSIENT_FOR(Window win, ScreenContext* scr);
  void Update_XA_WM_COLORMAP_WINDOWS(Window win, ScreenContext* scr);
  void Update_XA_WM_PROTOCOLS(Window win);
  void Set_XA_WM_STATE(Window win, int state);
  void GetPreviousWM_STATE(Window win);
  void CalcGravityOffset(int& xoff, int& yoff);
  int Open(ScreenContext* scr);
  int OpenIcon(ScreenContext* scr);
  void Close();
  void UnDecorateWindow(int reason);
  void ForceShow(int ic);
  void UnforceShow(int ic);
  void Iconify();
  void Deiconify();
  void MaybeMapUnmap();
  void InitialMap();
  void EventHandler(XEvent* evt);
  void ConfigureRequestEventHandler(XConfigureRequestEvent* evt);
  GroupHead* GetGroupHead(Window group_leader);
  GroupHead* WindowGroupHead();
  void DeleteGroupHead(Window group_leader);
  void AddWindowToGroupLeader(Window group_leader);
  void RemoveWindowFromGroup();
  void SetColormapFocus();
  void SetColormapFocusIndex(int ind);
  Window ColormapIndexWindow();
  int DeleteMessageOk() { return props->wm_delete_window; };
  int SaveYourselfMessageOk() { return props->wm_save_yourself; };
  int TakeFocusMessageOk() { return props->wm_take_focus; };
  void send_protocol_message(Atom protocol, int data_size, Card32* data);
  class Decoration* Deco() { return deco; };
  class Decoration* Icon() { return icon; };
  void SetDeco(class Decoration* d) { deco = d; };
  void SetIcon(class Decoration* d) { icon = d; };
  class IClient* Client() { return client; };
  class InnerDeco* Inner() { return (client ? (InnerDeco*)client : (InnerDeco*)menu); };
  Window InnerWin() { return (client ? client->XClient() : menu->Xwin()); };
  XWMHints* WmHints() { return &props->wm_hints; };
  XWMHints* OldWmHints() { return &props->old_wm_hints; };
  XSizeHints* NormalHints() { return &props->normal_hints; };
  SCM ClientClass() { return props->clientclass; };	
  SCM ClientName() { return props->clientname; };
  SCM MachineName() { return props->machinename; };
  SCM WindowName() { return props->windowname; };
  SCM IconName() { return props->iconname; };
  Window TransientFor() { return props->transient_for; };
  ClientWindow* Next() { return next; };
  int MappedWin() { return mapped_win; };
  int MappedIcon() { return mapped_icon; };
  int Iconified() { return iconified; };
  static void ClearGroupTable();
  int BlockClose(int val) { int old = (opened & 2); if (val) opened |= 2; else opened &= ~2; return old; };
  int BlockedClose() { return (opened & 2); };
  int IsClosing() { return (opened & 12); };
  int PendingClose() { return (opened & 8); };
protected:
  ClientWindow *previous;	// doubly linked list of windows 
  ClientWindow *next;
  class IClient* client;  	// the inner window 
  class IMenu* menu;	  	// or the inner menu 
  class Decoration* deco;
  class Decoration* icon;
  CachedProperties* props;	// X properties cached in common 
  Colormap colormap;		// colormap for main window 
  unsigned mapped_win : 1;	// flag if window is mapped 
  unsigned mapped_icon : 1;	// flag if icon is mapped 
  unsigned mapped_inner : 1;	// flag if inner client is mapped 
  unsigned iconified : 1;       // flag if window is iconified
  unsigned opened : 4;          // flags if window has finished opening/closing
  static GroupHead* grouptable[GROUPTABLELEN];
};

ClientWindow* DecorateWindow(Window window, ScreenContext* scr, SCM dproc, int newwin);
Decoration* RealizeIconWindow(class Decoration* deco, SCM iproc);
void conform_to_hints(XSizeHints* hints, int* width, int* height);

*/

extern FILE* gwmlog;   // log file used for debugging

#define GWMLOG(str, obj) if(gwmlog) { fprintf(gwmlog, str, obj); fflush(gwmlog); }

/*
 * Updates the sizehints for consitency with themselves and the screen dims.
 */
void CheckConsistency(XSizeHints* hints)
{
    if((hints->flags & PMinSize) && !(hints->flags & PBaseSize)) {
	hints->base_width = hints->min_width;
	hints->base_height = hints->min_height;
    } else if((hints->flags & PResizeInc) && !(hints->flags & PBaseSize)) {
	hints->base_width = 0;
	hints->base_height = 0;
    }
    if (hints->flags & PResizeInc && 
	(hints->width_inc <= 0 || hints->height_inc <= 0)) {
	hints->height_inc = hints->width_inc = 1;
    }
}

void conform_to_hints(XSizeHints* hints, int* width, int* height)
{
    if (hints->flags & PMinSize) {
	if (*width < hints->min_width)
	    *width = hints->min_width;
	if (*height < hints->min_height)
	    *height = hints->min_height;
    }
    if (hints->flags & PMaxSize) {
	if (*width > hints->max_width)
	    *width = hints->max_width;
	if (*height > hints->max_height)
	    *height = hints->max_height;
    }
    if (hints->flags & PAspect) {
    // I don't understand this code, but it seems wrong / aho
	if (hints->min_aspect.y * (*width)
	    < hints->min_aspect.x * (*height)) {
          //  if (xdir == 1) {
		*height = ((*width) * hints->min_aspect.y)
		    / hints->min_aspect.x;
          //  } else {
          //      *width = ((*height) * hints->min_aspect.x)
          //          / hints->min_aspect.y;
          //  }
	}
	if (hints->max_aspect.y * (*width)
	    > hints->max_aspect.x * (*height)) {
	  //  if (ydir == 1) {
		*width = ((*height) * hints->max_aspect.x)
		    / hints->max_aspect.y;
	  //  } else {
	  //      *height = ((*width) * hints->max_aspect.y)
	  //          / hints->max_aspect.x;
	  //  }
	}
    }
    if (hints->flags & PResizeInc) {
	*width = hints->base_width + hints->width_inc *
	    (((*width) - hints->base_width) / hints->width_inc);
	*height = hints->base_height + hints->height_inc *
	    (((*height) - hints->base_height) / hints->height_inc);
    }
}

/* routines */

ClientWindow::ClientWindow(Window win, ScreenContext* scr, XWindowAttributes* wa)
{
  client = new IClient(win, this, scr);
  scm_gc_protect_object(client->scm()); // prevent GC during creation
  menu = 0;
  previous = NULL;
  next = NULL;
  deco = NULL;
  icon = NULL;
  mapped_win = 0;
  mapped_icon = 0;
  mapped_inner = 1;
  iconified = 0;
  opened = 0;
  props = new CachedProperties();
  UpdateCachedProperties(win, scr);
  if (wa)
    colormap = wa->colormap;	/* update attributes */
//  printf("New window: %s\n", scm_i_string_chars(props->clientname));
//  printf("  state: %ld %d\n", props->wm_hints.flags, props->wm_hints.initial_state);
//  printf("  size flags: %s%s%s%s%s%s%s%s%s\n", (props->normal_hints.flags & 1 ? "USPosition " : ""), (props->normal_hints.flags & 2 ? "USSize " : ""), (props->normal_hints.flags & 4 ? "PPosition " : ""), (props->normal_hints.flags & 8 ? "PSize " : ""), (props->normal_hints.flags & 16 ? "PMinSize " : ""), (props->normal_hints.flags & 32 ? "PMaxSize " : ""), (props->normal_hints.flags & 64 ? "PResizeInc " : ""), (props->normal_hints.flags & 128 ? "PAspect " : ""), (props->normal_hints.flags & 256 ? "PBaseSize " : ""), (props->normal_hints.flags & 512 ? "PGravity " : ""));
//  printf("  obsolete: x=%d, y=%d, width=%d, height=%d\n", props->normal_hints.x, props->normal_hints.y, props->normal_hints.width, props->normal_hints.height);
//  printf("  base: width=%d [%d, %d, %d], height=%d [%d, %d, %d]\n", props->normal_hints.base_width, props->normal_hints.min_width, props->normal_hints.max_width, props->normal_hints.width_inc, props->normal_hints.base_height, props->normal_hints.min_height, props->normal_hints.max_height, props->normal_hints.height_inc);
  client->ConsiderClientSize(props, (GWM_new_window_id == win));
  scm_gc_unprotect_object(client->scm());
}

ClientWindow::ClientWindow(IMenu* mn, ScreenContext* scr, XWindowAttributes* wa)
{
  client = 0;
  menu = mn;
  menu->initialize(this);
  previous = NULL;
  next = NULL;
  deco = NULL;
  icon = NULL;
  mapped_win = 0;
  mapped_icon = 0;
  mapped_inner = 1;
  iconified = 0;
  opened = 0;
  props = new CachedProperties();
  UpdateCachedProperties(mn->Xwin(), scr);
  if (wa)
    colormap = wa->colormap;	/* update attributes */
}

ClientWindow::~ClientWindow()
{
  if (props) {
    FreeCachedProperties();
    delete props;
  }
}

/*
 * To decorate a window, we must:
 * 	- see if it is decorable
 * 	- get all info about it
 * 	- decorate it via NewClientWindow
 */

ClientWindow* DecorateWindow(Window window, ScreenContext* scr, SCM dproc, int newwin)
{
  ClientWindow* cw;
  Decoration* wob;
  XWindowAttributes wa;
  SCM desc, inner;
  int oldbc;
  unsigned int oldnwi;

  wa.override_redirect = 0;
  /* if window has died meanwhile, abort */
  if (TrapXErrors(XGetWindowAttributes(dpy, window, &wa))
      || wa.override_redirect) /* do not manage override_redirect wins */
    return NULL;

  oldnwi = GWM_new_window_id;
  if (newwin) GWM_new_window_id = window;
  /* be careful, we might have decorated it in beetween */
  if((cw = LookUpClient(window))) {
    if (cw->Deco()) {
      GWM_new_window_id = oldnwi;
      return cw;
    }
    inner = cw->Client()->scm();
  } else if ((wob = LookUpDeco(window))) {
    if (wob->Type() != MenuStatus || wob->Valid() != -1) {
      GWM_new_window_id = oldnwi;
      return NULL;
    }
    cw = new ClientWindow((IMenu*) wob, scr, &wa);
    inner = wob->scm();
  } else {
    cw = new ClientWindow(window, scr, &wa);
    inner = cw->Client()->scm();
  }
  if (!cw->InnerWin()) { // Already dead
    delete cw;
    GWM_new_window_id = oldnwi;
    return NULL;
  }
  oldbc = cw->BlockClose(1);
  if (!dproc)
    dproc = SCM_VARIABLE_REF(v_describe_window);
  if (dproc == SCM_UNDEFINED) {
    if (GWM_WidgetMode) {
      XSetWindowAttributes swa;
      swa.override_redirect = 0;
      XChangeWindowAttributes(dpy, cw->InnerWin(), CWOverrideRedirect, &swa);
    } else
      gwm_warning("\"describe-window\" is undefined, using empty decoration",
                  0);
    desc = inner;
  } else {
    SetContext(scr);
    desc = gwm_apply1_catch(dproc, inner);
    if (!WLDECOP(desc)) {
      gwm_warning("the return value of \"describe-window\": ~S\n    is not a window decoration, using empty decoration",
                  desc);
      desc = inner;
    }
  }
  cw->SetDeco(WL_DECO(desc));
  if (!cw->Open(scr)) {
    if (cw->PendingClose() || !cw->InnerWin()) {  // Already dead
      if (!oldbc && cw->PendingClose())
        cw->Close();
      else
        cw->BlockClose(oldbc);
      GWM_new_window_id = oldnwi;
      return NULL;
    } else if (cw->Deco() == cw->Inner()) {   // Try again
      Decoration* tmp = cw->Deco();
      cw->SetDeco(cw->Inner());
      if (!cw->Open(scr)) {
        if (!oldbc && cw->PendingClose())
          cw->Close();
        else
          cw->BlockClose(oldbc);
        GWM_new_window_id = oldnwi;
        return NULL;
      }
      gwm_warning("failed to open deco: ~S\n    using empty decoration.", tmp->scm());
      cw->BlockClose(oldbc);
      GWM_new_window_id = oldnwi;
      return cw;
    }
  } else {
    cw->BlockClose(oldbc);
    GWM_new_window_id = oldnwi;
    return cw;
  }
}

/*  The icon window is realized only on the first call to "iconify-window" or
 * "window-icon".
 * return icon or 0 if error
 */

Decoration* RealizeIconWindow(Decoration* deco, SCM iproc)
{
  SCM icondeco = SCM_UNDEFINED;
  ClientWindow* cw = deco->Win();
  int oldbc = cw->BlockClose(1);
  if (!iproc && !cw->Client() && cw->Inner())
    iproc = ((IMenu*) cw->Inner())->IconProcedure();
  if (!iproc)
    iproc = SCM_VARIABLE_REF(v_describe_icon);
  if (iproc == SCM_UNDEFINED) {
    if (!GWM_WidgetMode)
      gwm_warning("\"describe-icon\" is undefined, using empty decoration",
                  0);
  } else {
    SetContext(deco->Screen());
    icondeco = gwm_apply1_catch(iproc, deco->scm());
    if (!WLDECOP(icondeco)) {
      gwm_warning("the return value of \"describe-icon\": ~S\n    is not an icon decoration, using empty decoration",
                  icondeco);
    }
  }
  if (!WLDECOP(icondeco))
    icondeco = empty_deco()->scm();
  cw->SetIcon(WL_DECO(icondeco));
  if (cw->OpenIcon(deco->Screen())) {
    cw->BlockClose(oldbc);
    return WL_DECO(icondeco);
  } else if (!cw->PendingClose()) {   // Try again
    cw->SetIcon(empty_deco());
    if (cw->OpenIcon(deco->Screen())) {
      gwm_warning("Failed to open icon deco: ~A, using empty decoration.", icondeco);
      cw->BlockClose(oldbc);
      return cw->Icon();
    }
  }
  if (!oldbc && cw->PendingClose())
    cw->Close();
  else
    cw->BlockClose(oldbc);
  return 0;
}

Decoration* ClientWindow::MakeIconWindow()
{
  Window dummy_win;
  IClient* ic;
  if (props->wm_hints.flags & IconWindowHint &&
      !TrapXErrors(XGetTransientForHint(dpy,
                                        props->wm_hints.icon_window,
                                        &dummy_win))) {
    ic = LookUpInnerClient(props->wm_hints.icon_window);
    if (!ic)
      ic = new IClient(props->wm_hints.icon_window, this, deco->Screen());
    if (ic->XClient())
      return ic;
    else
      return 0;
  } else
    return 0;
}

/*
 * updates now the properties cached in the ClientWindow structure and icon
 */
void ClientWindow::UpdateCachedProperties(Window window, ScreenContext* scr)
{
  char* machname;
  XClassHint classhints;
  Atom actualtype;
  int actualformat;
  unsigned long nitems;
  unsigned long bytesafter;

  classhints.res_name = classhints.res_class = NULL;
  if (XGetWindowProperty(dpy, window, XA_WM_CLIENT_MACHINE, 0,
                         MAX_TEMP_STRING_SIZE, False, XA_STRING, 
                         &actualtype, &actualformat, &nitems, &bytesafter, 
                         (unsigned char **) &machname)) {
    machname = 0;
  }
  if (machname) {
    char *p = machname;
    for (; *p && *p != '.'; p++); 	/* truncate to first dot field */
    *p = '\0';
    props->machinename = scm_makfrom0str(machname);
    scm_gc_protect_object(props->machinename);
    XFree(machname);
  } else
    props->machinename = DefaultMachineName;
  if (XGetClassHint(dpy, window, &classhints)) {
    if (classhints.res_class) {
      props->clientclass = scm_makfrom0str(classhints.res_class);
      scm_gc_protect_object(props->clientclass);
      XFree(classhints.res_class);
    } else
      props->clientclass = DefaultClientClass;
    if (classhints.res_name) {
      props->clientname = scm_makfrom0str(classhints.res_name);
      scm_gc_protect_object(props->clientname);
      XFree(classhints.res_name);
    } else
      props->clientname = DefaultClientName;
  } else {
    props->clientclass = DefaultClientClass;
    props->clientname = DefaultClientName;
  }
  props->windowname = 0;
  props->iconname = 0;
  props->client_group = 0;
  props->colormap_windows_size = 0;
  props->colormap_windows = 0;
  props->wm_state = WM_STATE_Normal;
  /* update all variable props */
  Update_XA_WM_NAME(window);
  Update_XA_WM_ICON_NAME(window);
  Update_XA_WM_HINTS(window, 0);
  Update_XA_WM_NORMAL_HINTS(window);
  Update_XA_WM_TRANSIENT_FOR(window, scr);
  Update_XA_WM_COLORMAP_WINDOWS(window, scr);
  Update_XA_WM_PROTOCOLS(window);
  
  GetPreviousWM_STATE(window);	/* get the previous WM_STATE if any */
}

void ClientWindow::FreeCachedProperties()
{
  if (props->windowname && props->windowname != DefaultWindowName)
    scm_gc_unprotect_object(props->windowname);
  if (props->iconname && props->iconname != DefaultIconName)
    scm_gc_unprotect_object(props->iconname);
  if (props->clientclass != DefaultClientClass)
    scm_gc_unprotect_object(props->clientclass);
  if (props->clientname != DefaultClientName)
    scm_gc_unprotect_object(props->clientname);
  if (props->machinename != DefaultMachineName)
    scm_gc_unprotect_object(props->machinename);
  if (props->colormap_windows)
    delete [] props->colormap_windows;
}


/*
 * update ONE property
 * cw MUST be a window, not an icon
 */
void ClientWindow::UpdateCachedProperty(Atom property_atom, ScreenContext* scr)
{
  Window win;
  if (!client || client->Valid() == 4)
    return;
  win = InnerWin();
  switch (property_atom) {		/* predefined atoms */
  case XA_WM_NAME:
    Update_XA_WM_NAME(win);
    break;
  case XA_WM_ICON_NAME:
    Update_XA_WM_ICON_NAME(win);
    break;
  case XA_WM_HINTS:
    Update_XA_WM_HINTS(win, 1);
    break;
  case XA_WM_NORMAL_HINTS:
    Update_XA_WM_NORMAL_HINTS(win);
    break;
  case XA_WM_TRANSIENT_FOR:
    Update_XA_WM_TRANSIENT_FOR(win, scr);
    break;
  default:				/* non-predefined props */
    if (property_atom == XA_WM_COLORMAP_WINDOWS)
      Update_XA_WM_COLORMAP_WINDOWS(win, scr);
    else if (property_atom == XA_WM_PROTOCOLS)
      Update_XA_WM_PROTOCOLS(win);
  }
}

/* individual methods to update or create cached properties
 */

#ifdef HAVE_XGETWMNAME

void ClientWindow::Update_XA_WM_NAME(Window win)
{
  char *name = 0;
  XTextProperty  text_prop;
  char **list_return = 0;
  int count_return;
  if (XGetWMName(dpy, win, &text_prop)) {
    /* incantation from twm/add_window.c */
    if (text_prop.value) 
      text_prop.nitems = strlen((char *)text_prop.value);
    if (XmbTextPropertyToTextList(dpy, &text_prop, 
                                  &list_return, &count_return) == Success)
      if (list_return && *list_return)
        name = *list_return;
    if (text_prop.value)
      XFree(text_prop.value);
  }
  if (props->windowname && props->windowname != DefaultWindowName)
    scm_gc_unprotect_object(props->windowname);
  if (!name || (*name == '\0'))
    props->windowname = DefaultWindowName;
  else {
    props->windowname = scm_makfrom0str(name);
    scm_gc_protect_object(props->windowname);
  }
  if (list_return) XFreeStringList(list_return);
}

void ClientWindow::Update_XA_WM_ICON_NAME(Window win)
{
  char *name = 0;
  XTextProperty  text_prop;
  char **list_return = 0;
  int count_return;
  if (XGetWMIconName(dpy, win, &text_prop)) {
    /* incantation from twm/add_window.c */
    if (text_prop.value)
      text_prop.nitems = strlen((char *)text_prop.value);
    if ( XmbTextPropertyToTextList(dpy, &text_prop,
                                   &list_return, &count_return) == Success)
      if (list_return && (*list_return))
        name = *list_return;
    if (text_prop.value)
      XFree(text_prop.value);
  }    
  if (props->iconname && props->iconname != DefaultIconName)
    scm_gc_unprotect_object(props->iconname);
  if (!name || (*name == '\0')) {
    props->iconname = DefaultIconName;
  } else {
    props->iconname = scm_makfrom0str(name);
    scm_gc_protect_object(props->iconname);
  }
  if (list_return) XFreeStringList(list_return);
}

#else

void ClientWindow::Update_XA_WM_NAME(Window win)
{
  char *name = 0;
  XFetchName(dpy, win, &name);
  if (props->windowname && props->windowname != DefaultWindowName)
    scm_gc_unprotect_object(props->windowname);
  if (!name || (*name == '\0'))
    props->windowname = DefaultWindowName;
  else {
    props->windowname = scm_makfrom0str(name);
    scm_gc_protect_object(props->windowname);
    XFree(name);
  }
}

void ClientWindow::Update_XA_WM_ICON_NAME(Window win)
{
  char *name = 0;
  XGetIconName(dpy, win, &name);
  if (props->iconname && props->iconname != DefaultIconName)
    scm_gc_unprotect_object(props->iconname);
  if (!name || (*name == '\0')) {
    props->iconname = DefaultIconName;
  } else {
    props->iconname = scm_makfrom0str(name);
    scm_gc_protect_object(props->iconname);
    XFree(name);
  }
}

#endif /* X11R6 or later */

void ClientWindow::Update_XA_WM_HINTS(Window win, int backup)
{
  XWMHints *windowhints, *wm_hints;
  if (backup)
    bcopy(&(props->wm_hints),
          &(props->old_wm_hints), sizeof(XWMHints));
  if ((windowhints = XGetWMHints(dpy, win))) {
    bcopy(windowhints, wm_hints = &(props->wm_hints),
          sizeof(XWMHints));

    /* window groups */
    if (windowhints->flags & WindowGroupHint) {
      if (windowhints->window_group) 
        AddWindowToGroupLeader(windowhints->window_group);
      else if (props->client_group)
        RemoveWindowFromGroup();
    }

    /* check validity of hints */
    if (wm_hints->flags & IconWindowHint && !(wm_hints->icon_window))
      wm_hints->flags &= ~IconWindowHint;
    if (wm_hints->flags & IconPixmapHint && !(wm_hints->icon_pixmap))
      wm_hints->flags &= ~IconPixmapHint;
    if (wm_hints->flags & IconMaskHint && !(wm_hints->icon_mask))
      wm_hints->flags &= ~IconMaskHint;

    XFree(windowhints);
  } else {
    props->wm_hints.flags = 0;
  }
}

void ClientWindow::Update_XA_WM_NORMAL_HINTS(Window win)
{
  long supplied = 0;
  if (!XGetWMNormalHints(dpy, win,
                         &(props->normal_hints),
                         &supplied)) {
    props->normal_hints.flags = 0;
  }
  if (supplied & (PBaseSize|PWinGravity)) {
    props->new_normal_hints = 1;
  } else {
    props->new_normal_hints = 0;
  }
  CheckConsistency(&(props->normal_hints));
}

void ClientWindow::Update_XA_WM_TRANSIENT_FOR(Window win, ScreenContext* scr)
{
  Window transient_for = 0;
  if (TrapXErrors(XGetTransientForHint(dpy, win, &transient_for)))
    props->transient_for = 0;
  else if (transient_for &&
           transient_for != scr->Root() &&
           transient_for != win)
    props->transient_for = transient_for;
  else
    props->transient_for = 0;
}

void ClientWindow::Update_XA_WM_COLORMAP_WINDOWS(Window win, ScreenContext* scr)
{
  Atom actualtype;
  int actualformat;
  unsigned long nitems, i;
  unsigned long bytesafter;
  Window *colormap_windows = NULL, *old_colormap_windows;
  int res;

  TrapXErrors(res = XGetWindowProperty(dpy, win, XA_WM_COLORMAP_WINDOWS, 0,
                                       WM_COLORMAP_WINDOWS_PROP_Length, False,
                                       XA_WM_COLORMAP_WINDOWS, &actualtype,
                                       &actualformat, &nitems, &bytesafter,
                                       (unsigned char **) &colormap_windows));
  if (res == Success && nitems && actualtype != None && actualformat == 32) {
    old_colormap_windows = props->colormap_windows;
    props->colormap_windows = new Window[nitems+1];
    props->colormap_windows_size = 1;
    props->colormap_windows[0] = win;
    for(i = 0; i< nitems; i++)
      if (colormap_windows[i] != win && !LookUpClient(colormap_windows[i])) {
        props->colormap_windows[props->colormap_windows_size++] = colormap_windows[i];
        if (colormap_windows[i] != win) {
          XSelectInput(dpy, colormap_windows[i], ColormapChangeMask);
        }
      }
    if (props->colormap_windows_size == 1) {
      XFree(props->colormap_windows);
      props->colormap_windows = 0;
      props->colormap_windows_size = 0;
    }
    props->colormap_windows_index = 0;
    scr->AnnounceColormap(this, props->colormap_windows[0]);
    if(old_colormap_windows)
      XFree(old_colormap_windows);
  }
  if(colormap_windows && res == Success)
    XFree(colormap_windows);
}

void ClientWindow::Update_XA_WM_PROTOCOLS(Window win)
{
  Atom actualtype;
  int actualformat;
  unsigned long nitems, i;
  unsigned long bytesafter;
  Window *protocols = NULL;
  int res;

  TrapXErrors(res = XGetWindowProperty(dpy, win, XA_WM_PROTOCOLS, 0,
                                       WM_PROTOCOLS_PROP_Length, False,
                                       XA_ATOM, &actualtype, &actualformat,
                                       &nitems, &bytesafter,
                                       (unsigned char **) &protocols));
  if (res == Success && nitems && actualtype != None && actualformat == 32) {
    props->wm_take_focus = 0;
    props->wm_save_yourself = 0;
    props->wm_delete_window = 0;
    for (i = 0; i < nitems; i++) {
      if (protocols[i] == XA_WM_TAKE_FOCUS)
        props->wm_take_focus = 1;
      else if (protocols[i] == XA_WM_SAVE_YOURSELF)
        props->wm_save_yourself = 1;
      else if (protocols[i] == XA_WM_DELETE_WINDOW)
        props->wm_delete_window = 1;
    }
  }
  if (protocols && res == Success)
    XFree(protocols);
}

void ClientWindow::Set_XA_WM_STATE(Window win, int state)
{
  struct _WM_STATE_PROP wm_state;
  wm_state.state = state;
  if (wm_state.state != props->wm_state) {
    props->wm_state = wm_state.state;
    wm_state.icon = (icon ? icon->Xwin() : 0);
    XChangeProperty(dpy, win, XA_WM_STATE, XA_WM_STATE,
                    32, PropModeReplace, (unsigned char *)&wm_state,
                    WM_STATE_PROP_Length);
  }
}

/* first time a window is mapped, decode the WM_STATE from previous wm
 */
void ClientWindow::GetPreviousWM_STATE(Window win)
{
  Atom actualtype;
  int actualformat;
  unsigned long nitems;
  unsigned long bytesafter;
  WM_STATE_PROP wm_state;

  if ((GWM_new_window_id != win) &&
      Success == XGetWindowProperty(dpy, win, XA_WM_STATE, 0,
                                    WM_STATE_PROP_Length, False,
                                    XA_WM_STATE, &actualtype,
                                    &actualformat, &nitems, &bytesafter,
                                    (unsigned char **)&wm_state)
      && nitems && wm_state) {
    props->wm_hints.flags |= StateHint;
    props->wm_hints.initial_state = wm_state->state;
    XFree(wm_state);
  }
  if (props->wm_hints.flags & StateHint) { /* Also set by Update_XA_WM_HINTS */
    switch (props->wm_hints.initial_state) {
    case WM_STATE_Iconified:
      iconified = 1;
      break;
    case WM_STATE_Withdrawn:	/* ERROR! buggy clients */
      /* we map, ignoring the bogus hint but printing a warning */
      gwm_warning("window ~A has a bogus hint WM_STATE_Withdrawn for initial state", 
                  props->windowname);
      break;
    }
  }
}

/* Gravity: 123
 *          456
 *          789
 * (Gravity - 1) % 3: x: 0 1 2
 * (Gravity - 1) / 3: y: 0 1 2
 * 1 is center, -1 is just client mustn't move
 */
void ClientWindow::CalcGravityOffset(int& xoff, int& yoff)
{
  int xgrav, ygrav;
  InnerDeco* inner;
  int grav = NormalHints()->win_gravity;
  if (NormalHints()->flags & PWinGravity) {
    if ((grav <= ForgetGravity) || (grav >= StaticGravity))
      xgrav = ygrav = -1;
    else {
      xgrav = (grav - 1) % 3;
      ygrav = (grav - 1) / 3;
    }
  } else {
    xgrav = ygrav = 0;           /* NorthWest is default */
  }
  inner = Inner();
  if (!inner || inner->Top() != deco) {
    xoff = 0;
    yoff = 0;
    return;
  }
  if (xgrav == -1 || ygrav == -1) {	/* client don't move */
    int ulx, uly, d1, d2;
    inner->GetInnerDims(ulx, uly, d1, d2);
    xoff = ulx - inner->OriginalBorderwidth();
    yoff = uly - inner->OriginalBorderwidth();
  } else {
    switch(xgrav) {
    case 0:			/* west */
      xoff = 0;
      break;
    case 1:			/* center */
      xoff = deco->Borderwidth() - inner->Borderwidth() +
             (deco->Width() - inner->Width())/2; 
      break; 
    case 2:			/* east */
      xoff = (deco->Borderwidth() - inner->Borderwidth())*2 +
             deco->Width() - inner->Width(); 
      break;
    }
    switch(ygrav) {
    case 0:			/* north */
      yoff = 0;
      break;
    case 1:			/* center */
      yoff = deco->Borderwidth() - inner->Borderwidth() +
             (deco->Height() - inner->Height())/2; 
      break; 
    case 2:			/* south */
      yoff = (deco->Borderwidth() - inner->Borderwidth())*2 +
             deco->Height() - inner->Height();
      break;
    }
  }
}

/*
 * Here we add physically all the gadgets around a client window.
 * A new window is opened. (First Map Request, called from SetUpClient.)
 */
int ClientWindow::Open(ScreenContext* scr)
{
  int xoff, yoff, xpos, ypos;
  /* put window in window list */
  scr->RegisterWindow(this);
  xpos = Inner()->Xorig();
  ypos = Inner()->Yorig();
  CalcGravityOffset(xoff, yoff);	/* sets reparenting offsets */
  deco->updatepos(xpos-xoff, ypos-yoff);
  if (deco->check_free_menu())
    deco = ((IMenu*) deco)->unrealize();
  if (!deco->open(WindowStatus, this, scr)) {
    deco = Inner();   // dont touch the deco anymore
    scr->UnregisterWindow(this);
    return 0;
  }
  /* process pending events, but not for GWM menus */
  if (!GWM_is_starting && client != 0)
    GWM_ProcessEvents(0);

  /* execute "opening" code and map */
  InitialMap();
  if ((opened & 1) && !PendingClose())
    return 1;
  else {
    scr->UnregisterWindow(this);
    return 0;
  }
}

int ClientWindow::OpenIcon(ScreenContext* scr)
{
  if (icon && props->wm_hints.flags & IconPositionHint)
    icon->updatepos(props->wm_hints.icon_x, props->wm_hints.icon_y);
  else
    icon->updatepos(0, 0);
  if (icon->check_free_menu())
    icon = ((IMenu*) icon)->unrealize();
  if (!icon->open(IconStatus, this, scr)) {
    icon = NULL; 
    return 0;
  }
  /* process pending events, but not for GWM menus */
  if (!GWM_is_starting && client != 0)
    GWM_ProcessEvents(0);
  /* execute "opening" code */
  if (icon->Valid() > 0) {
    icon->execopen();
  }
  return (icon->Valid() > 0 && !PendingClose());
}

/* destroys all data concerning the window
 * suppose that the client window is already destroyed, so we do not need to
 * destroy it ourselves
 */
void ClientWindow::Close()
{
  ScreenContext* scr = deco->Screen();
  if (!(opened & 4)) {
    opened |= 4;
    if (props->client_group)
      RemoveWindowFromGroup();
    scr->DenounceColormap(this);
    /* remove from list */
    scr->UnregisterWindow(this);
    if ((opened & 1)) {
      if (deco->Valid() > 0) {
        ProcessGwmEvents();
        deco->execclose();
        deco->close(0);
        deco->updatepos(0, 0);
      }
      if (icon && icon->Valid() > 0) {
        ProcessGwmEvents();
        icon->execclose();
        icon->close(0);
        icon->updatepos(0, 0);
      }
    }
    opened = 0;
    delete this;
  }
}

void ClientWindow::UnDecorateWindow(int reason)
{
  // reason: 0 = closing, 1 = ending, 2 = redecorate
  Window win;
  int rubber;
  win = InnerWin();
  rubber = GWM_rubber_feedback;
  if (rubber) {
    UnDrawRubber();
    GWM_rubber_feedback = 0;
  }
  XSync(dpy, 0);
  XSetErrorHandler(NoXError);
  if (mapped_win) {
    CatchUnmapRelease(deco);
    XUnmapWindow(dpy, deco->Xwin());
    deco->issue_unmap_event();
    mapped_win = 0;
  }
  if (icon && mapped_icon) {
    CatchUnmapRelease(icon);
    XUnmapWindow(dpy, icon->Xwin());
    icon->issue_unmap_event();
    mapped_icon = 0;
  }
  if (client && (reason==1) && !mapped_inner)
    XMapWindow(dpy, win);
  else if (client && (reason==0))
    Set_XA_WM_STATE(win, WM_STATE_Withdrawn);
  if (!BlockedClose() || (reason==1)) {
    Close();
  } else {
    opened |= 8;
    Inner()->close(2);
  }
  XSync(dpy, 0);
  XSetErrorHandler(XError);
  if (rubber) {
    ReDrawRubber();
    GWM_rubber_feedback = 1;
  }
}

void ClientWindow::Iconify()
{
  if (!(opened & 1)) {
    props->wm_hints.flags &= ~StateHint;
    iconified = 1;
  } else {
    if (!icon)
      RealizeIconWindow(Inner(), 0);
    iconified = 1;
    MaybeMapUnmap();
    Set_XA_WM_STATE(InnerWin(), WM_STATE_Iconified);
  }
}

void ClientWindow::Deiconify()
{
  if (!(opened & 1)) {
    props->wm_hints.flags &= ~StateHint;
    iconified = 0;
  } else {
    iconified = 0;
    MaybeMapUnmap();
    Set_XA_WM_STATE(InnerWin(), WM_STATE_Normal);
  }
}

void ClientWindow::MaybeMapUnmap()
{
  int rubber = GWM_rubber_feedback;
  if (!(opened & 1))
    return;
  if (rubber) {
    UnDrawRubber();
    GWM_rubber_feedback = 0;
  }
  if (iconified) {
    if (deco->Hidden() != -1) {
      if (mapped_win) {
        CatchUnmapRelease(deco);
        XUnmapWindow(dpy, deco->Xwin());
        deco->issue_unmap_event();
      }
      if (mapped_inner)
        XUnmapWindow(dpy, InnerWin());
      mapped_win = 0;
      mapped_inner = 0;
    } else {
      if (!mapped_inner)
        XMapWindow(dpy, InnerWin());
      if (!mapped_win) {
        XMapWindow(dpy, deco->Xwin());
        deco->issue_map_event();
      }
      mapped_win = 1;
      mapped_inner = 1;
    }
    if (icon && icon->Hidden() != 1) {
      if (!mapped_icon) {
        XMapWindow(dpy, icon->Xwin());
        icon->issue_map_event();
      }
      mapped_icon = 1;
    } else if (icon) {
      if (mapped_icon) {
        CatchUnmapRelease(icon);
        XUnmapWindow(dpy, icon->Xwin());
        icon->issue_unmap_event();
      }
      mapped_icon = 0;
    }
  } else {
    if (icon && icon->Hidden() != -1) {
      if (mapped_icon) {
        CatchUnmapRelease(icon);
        XUnmapWindow(dpy, icon->Xwin());
        icon->issue_unmap_event();
      }
      mapped_icon = 0;
    } else if (icon) {
      if (!mapped_icon) {
        XMapWindow(dpy, icon->Xwin());
        icon->issue_map_event();
      }
      mapped_icon = 1;
    }
    if (!mapped_inner)
      XMapWindow(dpy, InnerWin());
    mapped_inner = 1;
    if (deco->Hidden() != 1) {
      if (!mapped_win) {
        XMapWindow(dpy, deco->Xwin());
        deco->issue_map_event();
      }
      mapped_win = 1;
    } else {
      if (mapped_win) {
        CatchUnmapRelease(deco);
        XUnmapWindow(dpy, deco->Xwin());
        deco->issue_unmap_event();
      }
      mapped_win = 0;
    }
  }    
  if (rubber) {
    ReDrawRubber();
    GWM_rubber_feedback = 1;
  }
}

void ClientWindow::InitialMap()
{
  if (!(opened & 1) && deco->Valid() > 0) {
    deco->execopen();
    if (!IsClosing()) {
      if ((props->wm_hints.flags & StateHint) && // Client request, use callback
          (props->wm_hints.initial_state == WM_STATE_Iconified)) {
        SetContext(deco->Screen());
        gwm_apply1_catch(SCM_VARIABLE_REF(v_iconify_window), deco->scm());
      } 
      if (iconified) {         // User call or client request, catch up wm_state
        if (!icon)
          RealizeIconWindow(Inner(), 0);
        Set_XA_WM_STATE(InnerWin(), WM_STATE_Iconified);
      }
    }
    opened |= 1;
    MaybeMapUnmap();
  }
}

void ClientWindow::EventHandler(XEvent* evt)
{
  switch (evt->type) {

  case MapNotify:
  case ConfigureNotify:
  case GravityNotify:
  case CirculateNotify:
    break;

  case MapRequest:            /* client is mapping its window */
    if (opened & 1) {
      if(!mapped_win) {
        if (iconified) {
          gwm_apply1_catch(SCM_VARIABLE_REF(v_de_iconify_window), icon->scm());
        } else {
          // Should we obey this - client want to map a hidden window
          // Map(); 
        }
      }
    } else {				/* initial map */
      InitialMap();
    }
    break;

  case UnmapNotify:           /* Withdraw the window */
    if (evt->xunmap.window == InnerWin()
        && mapped_win) {
      UnDecorateWindow(0);
    }
    break;

  case ConfigureRequest:
    ConfigureRequestEventHandler((XConfigureRequestEvent*) evt);
    break;

  case DestroyNotify:
    if (evt->xdestroywindow.window == InnerWin()
        && evt->xdestroywindow.event == InnerWin()) {
      UnDecorateWindow(0);
    }
    break;

  case ColormapNotify:		/* if is the active one, re-install */
    if (evt->xcolormap.c_new) {
      colormap = evt->xcolormap.colormap;
      deco->Screen()->AnnounceColormap(this, colormap);
    }				/* else install/uninstall result */
    break;

  case ClientMessage:		/* iccc: for iconifying window */
    if (evt->xclient.message_type == XA_WM_CHANGE_STATE
        && evt->xclient.data.l[0] == IconicState) {
      gwm_apply1_catch(SCM_VARIABLE_REF(v_iconify_window), deco->scm());
      /* iccc: for deleting window */
    } else if (evt->xclient.message_type == XA_WM_PROTOCOLS
               && evt->xclient.data.l[0] == (long) XA_WM_DELETE_WINDOW) {
      UnDecorateWindow(0);
    } else {
      deco->EventHandler(evt);
    }
    break;

  case PropertyNotify:
    UpdateCachedProperty(evt->xproperty.atom, deco->Screen());
    deco->EventHandler(evt);
    break;

  default:

    if (GWM_ShapeExtension &&
        evt->type == GWM_ShapeEventBase + ShapeNotify) {
      XShapeEvent* shev = (XShapeEvent*) evt;
      if (shev->kind == ShapeBounding &&
          shev->window == InnerWin()) {
        client->change_shape(shev->shaped);
      }
      break;
    }

    if (client)
      client->EventHandler(evt);
  }
}

/*
 * ConfigureRequestEventHandler is used to decode what operation is really
 * intended by the configure event to only perform the appropriate action
 */
void ClientWindow::ConfigureRequestEventHandler(XConfigureRequestEvent* evt)
{
  int ulx, uly, lrx, lry, bwd, xpos, ypos, width, height;
  if (evt->value_mask & CWBorderWidth) {
    bwd = Inner()->Borderwidth();
    client->ReconfBorderwidth(evt->border_width);
    bwd -= Inner()->Borderwidth();
  } else
    bwd = 0;
  if (evt->value_mask & (CWWidth | CWHeight) || bwd != 0) {
    Inner()->GetInnerDims(ulx, uly, lrx, lry);
    width = (evt->value_mask & CWWidth ?
             evt->width + deco->Width() + 2*deco->Borderwidth() - lrx + ulx - 1 :
             deco->Width() + 2*deco->Borderwidth() + 2*bwd);
    height = (evt->value_mask & CWHeight ?
              evt->height + deco->Height() + 2*deco->Borderwidth() - lry + uly - 1 :
              deco->Height() + 2*deco->Borderwidth() + 2*bwd);
    if (evt->value_mask & (CWX | CWY)) {
      xpos = (evt->value_mask & CWX ?
              evt->x - ulx + Inner()->Borderwidth() :
              deco->Xpos());
      ypos = (evt->value_mask & CWY ?
              evt->y - uly + Inner()->Borderwidth() :
              deco->Ypos());
      gwm_applyN_catch(SCM_VARIABLE_REF(v_move_resize_window),
                       SCM_LIST5(deco->scm(), gh_int2scm(xpos), gh_int2scm(ypos),
                                 gh_int2scm(width), gh_int2scm(height)));
    } else
      gwm_applyN_catch(SCM_VARIABLE_REF(v_resize_window),
                       SCM_LIST3(deco->scm(), gh_int2scm(width), gh_int2scm(height)));
  } else if (evt->value_mask & (CWX | CWY)) { /* just move */
    Inner()->GetInnerDims(ulx, uly, lrx, lry);
    xpos = (evt->value_mask & CWX ?
            evt->x - ulx + Inner()->Borderwidth() :
            deco->Xpos());
    ypos = (evt->value_mask & CWY ?
            evt->y - uly + Inner()->Borderwidth() :
            deco->Ypos());
    gwm_applyN_catch(SCM_VARIABLE_REF(v_move_window),
                     SCM_LIST3(deco->scm(), gh_int2scm(xpos), gh_int2scm(ypos)));
  }
  if (evt->value_mask & CWStackMode) {
    ClientWindow* sibling = 0;
    Decoration* dec;
    if (evt->value_mask & CWSibling) {	/* % to a sibling */
      sibling = LookUpClient(evt->above);
      if (!sibling)
        dec = LookUpDeco(evt->above), sibling = (dec ? dec->Win() : 0);
    }
    if (sibling && sibling != this) {
      switch (evt->detail) {
      case Above:
        gwm_apply2_catch(SCM_VARIABLE_REF(v_raise_window), deco->scm(), sibling->deco->scm());
        break;
      case Below:
        gwm_apply2_catch(SCM_VARIABLE_REF(v_lower_window), deco->scm(), sibling->deco->scm());
        break;
      default: ;
      }
    } else {
      switch (evt->detail) {
      case Above:
        gwm_apply1_catch(SCM_VARIABLE_REF(v_raise_window), deco->scm());
        break;
      case Below:
        gwm_apply1_catch(SCM_VARIABLE_REF(v_lower_window), deco->scm());
        break;
      default: ;
      }
    }
  }
}

/****************\
* 		 *
* window groups	 *
* 		 *
\****************/

/* Maintains the group list of a group_leader
 * A separate table is used, since group leader need not be
 * decorated windows. 
 */


GroupHead* ClientWindow::grouptable[GROUPTABLELEN];

GroupHead* ClientWindow::GetGroupHead(Window group_leader)
{
  GroupHead* head = grouptable[group_leader%GROUPTABLELEN];
  if (!group_leader) return 0;
  while (head && head->leader != group_leader)
    head = head->next;
  if (!head) {
    head = new GroupHead();
    head->leader = group_leader;
    head->group = scm_cons(SemiDecoratedWindow(group_leader), SCM_EOL);
    scm_gc_protect_object(head->group);
    head->next = grouptable[group_leader%GROUPTABLELEN];
    grouptable[group_leader%GROUPTABLELEN] = head;
  }
  return head;
}

GroupHead* ClientWindow::WindowGroupHead()
{
  GroupHead* head;
  Window group_leader = props->client_group;
  if (!group_leader)
    return 0;
  head = grouptable[group_leader%GROUPTABLELEN];
  while (head && head->leader != group_leader)
    head = head->next;
  return head;
}

void ClientWindow::DeleteGroupHead(Window group_leader)
{
  GroupHead* last = 0;
  GroupHead* head = grouptable[group_leader%GROUPTABLELEN];
  while (head && head->leader != group_leader)
    last = head, head = head->next;
  if (head) {
    if (last)
      last->next = head->next;
    else
      grouptable[group_leader%GROUPTABLELEN] = head->next;
    scm_gc_unprotect_object(head->group);
    delete head;
  }
}

void ClientWindow::AddWindowToGroupLeader(Window group_leader)
{
  GroupHead* head;
  SCM curr;
  if (props->client_group && props->client_group != group_leader)
    RemoveWindowFromGroup();
  props->client_group = group_leader;
  if (group_leader == InnerWin()) {
    head = WindowGroupHead();
    if (head && scm_is_integer(SCM_CAR(head->group)))
      SCM_SETCAR(head->group, client->scm());
  } else {
    head = GetGroupHead(group_leader);
    curr = SCM_CDR(head->group);
    while (curr != SCM_EOL && SCM_CAR(curr) != client->scm())
      curr = SCM_CDR(curr);
    if (curr != SCM_EOL)
      return;
    SCM_SETCDR(head->group, scm_cons(client->scm(), SCM_CDR(head->group)));
  }    
}

void ClientWindow::RemoveWindowFromGroup()
{
  GroupHead* head;
  SCM curr, last;
  head = WindowGroupHead();
  if (!head)
    return;
  if (head->leader == InnerWin()) {
    if (SCM_CDR(head->group) == SCM_EOL)
      DeleteGroupHead(head->leader);
    else if (!scm_is_integer(SCM_CAR(head->group)))
      SCM_SETCAR(head->group, gh_int2scm(InnerWin()));
  } else {
    curr = SCM_CDR(head->group);
    last = head->group;
    while (curr != SCM_EOL && SCM_CAR(curr) != client->scm())
      last = curr, curr = SCM_CDR(curr);
    if (curr != SCM_EOL)
      SCM_SETCDR(last, SCM_CDR(curr));
    if (SCM_CDR(head->group) == SCM_EOL)
      DeleteGroupHead(head->leader);
    props->client_group = 0;
  }
}
	    
void ClientWindow::ClearGroupTable()
{
  int i;
  for (i=0; i<GROUPTABLELEN; i++)
    grouptable[i] = 0;
}

/* Colormaps
 * colormap focus management:
 */

void ClientWindow::SetColormapFocus()
{
  deco->Screen()->SetColormapWindow(this, colormap);
  if (props->colormap_windows)
    props->colormap_windows_index = 0;
}

void ClientWindow::SetColormapFocusIndex(int ind)
{
  XWindowAttributes wa;
  Colormap current_colormap;
  if (deco->Screen()->ColormapWindow() == this && props->colormap_windows) {
    if (ind != -1) {		/* absolute index */
      props->colormap_windows_index = ind % props->colormap_windows_size;
      XGetWindowAttributes(dpy,
                           props->colormap_windows[props->colormap_windows_index],
                           &wa);
      deco->Screen()->AnnounceColormap(this, wa.colormap);
    } else {			/* increment index */
      XGetWindowAttributes(dpy,
                           props->colormap_windows[props->colormap_windows_index],
                           &wa);
      current_colormap = wa.colormap;
      for (ind = props->colormap_windows_index + 1;
           ind % props->colormap_windows_size != props->colormap_windows_index;
           ind++) {
        ind = ind % props->colormap_windows_size;
        XGetWindowAttributes(dpy, props->colormap_windows[ind], &wa);
        if (wa.colormap != current_colormap) {
          props->colormap_windows_index = ind;
          deco->Screen()->AnnounceColormap(this, wa.colormap);
          break;
        }
      }
    }
  }
}

Window ClientWindow::ColormapIndexWindow()
{
  if (props->colormap_windows_index)
    return props->colormap_windows[props->colormap_windows_index];
  else
    return 0;
}

void ClientWindow::send_protocol_message(Atom protocol, int data_size, Card32* data)
{
  XClientMessageEvent event;
  event.type = ClientMessage;
  event.window = InnerWin();
  event.message_type = XA_WM_PROTOCOLS;
  event.format = 32;
  event.data.l[0] = (long) protocol;
  event.data.l[1] = (long) GWMTime;
  data_size = Min(3, data_size);
  if (data_size)
    bcopy(data, &(event.data.l[2]), data_size * 4);
  if (data_size < 3)
    bzero(&(event.data.l[2 + data_size]), (3 - data_size) * 4);
  TrapXErrors(XSendEvent(dpy, event.window, False, 0, (XEvent *) &event));
}

void init_scm_client()
{
  ClientWindow::ClearGroupTable();
}

