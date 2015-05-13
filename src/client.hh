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
  void UnDecorateWindow(int remap);
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











































