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


extern long scm_tc16_deco;

struct Box {
    int             x, y;
    unsigned int    width, height;
    unsigned int    borderwidth;
    int shape_flags;
    int trans_back() { return shape_flags & 1; };
    int hole_back() { return shape_flags & 2; };
    int shaped_back() { return shape_flags & 3; };
    int trans_border() { return shape_flags & 4; };
    int hole_border() { return shape_flags & 8; };
    int shaped_border() { return shape_flags & 12; };
    int trans_parts() { return shape_flags & 16; };
    int hole_parts() { return shape_flags & 32; };
    int shaped_parts() { return shape_flags & 48; };
    int shaped() { return shape_flags & 47; };
    int shaped_hole() { return shape_flags & 42; };
    int shaped_trans() { return shape_flags & 5; };
    void set_trans_back(int v) { if (v) shape_flags |= 1; else shape_flags &= ~1;};
    void set_hole_back(int v) { if (v) shape_flags |= 2; else shape_flags &= ~2; };
    void set_trans_border(int v) { if (v) shape_flags |= 4; else shape_flags &= ~4; };
    void set_hole_border(int v) { if (v) shape_flags |= 8; else shape_flags &= ~8; };
    void set_trans_parts(int v) { if (v) shape_flags |= 16; else shape_flags &= ~16; };
    void set_hole_parts(int v) { if (v) shape_flags |= 32; else shape_flags &= ~32; };
    int nonzerosize() { return (width && height); };
    int onlyborder() { return (!(width && height) && borderwidth); };
};

struct Anchor {
    int numx;
    float outx1;
    float inx1;
    int offx1;
    float outx2;
    float inx2;
    int offx2;
    int numy;
    float outy1;
    float iny1;
    int offy1;
    float outy2;
    float iny2;
    int offy2;
};

class Decoration {
  friend class Bar;
  friend class Plug;
  friend class IMenu;
  friend class IClient;
  friend class UScreen;
public:
  Decoration(SCM ctx, int cn, const char* tag);
  Decoration();
  virtual ~Decoration();
  virtual void mark();
  virtual int print(SCM port) = 0;
  virtual SCM inspect(SCM ctxlst);
  virtual void modify(SCM ctx, int cn, const char* tag);
  virtual void execopen();
  virtual void execclose();
  virtual int open(int stat, class ClientWindow* cw, class ScreenContext* scr);
  virtual void semiopen(class ClientWindow* cw, class ScreenContext* scr);
  virtual void close(int flag);
  virtual int calclength(int dir, int delta, int& ext);
  virtual void setnaturalsize(short pdir) {};
  virtual void changepos(int x, int y);
  virtual void updatepos(int x, int y);
  virtual void updatesize(int xs, int ys) {};
  virtual void EventHandler(XEvent* evt);
  virtual void ReconfigureUp(int size, int pos, int shape, int inhibsh, int inhibrc) {};
  virtual void ReconfigureTurn(int inhibrc);
  virtual void ReconfigureDown() {};
  virtual void hide();
  virtual void show();
  virtual void force_show();
  virtual void unforce_show();
  virtual SCM get_bordercolor() { return bordercolor; };
  virtual int get_borderwidth() { return borderwidth; };
  virtual SCM get_background() { return background; };
  virtual SCM get_cursor() { return cursor; };
  virtual SCM get_anchor() { return anchor; };
  virtual SCM get_property() { return property; };
  virtual SCM get_behavior() { return behavior; };
  virtual SCM get_parts() { return SCM_BOOL_F; };
  virtual SCM get_part(int n) { return SCM_BOOL_F; };
  virtual int get_num_parts() { return -1; };
  virtual void set_bordercolor(SCM obj);
  virtual void set_borderwidth(SCM wdt);
  virtual void intern_set_borderwidth(int wdt);
  virtual void set_background(SCM obj);
  virtual void set_cursor(SCM obj);
  virtual void set_anchor(SCM obj);
  virtual int intern_set_anchor(SCM obj);
  virtual void set_property(SCM obj);
  virtual void set_behavior(SCM obj);
  virtual void set_part(SCM obj, int n) {};
  virtual void insert_part(SCM obj, int n) {};
  virtual void remove_part(int n) {};
  virtual void remove_part(Decoration* part) {};
  virtual void reorder_part(int n, int m) {};
  void set_active_grab(Decoration* par, class WlCursor* cursor, unsigned int specmask, int grab_kbd, int grab_cld, int grab_cnf, int grab_nofrz, int async);
  void remove_active_grab(int async);
  Window Xwin() { return hook; };
  void dimensions(Box* b);
  int Xpos() { return box.x; };
  int Ypos() { return box.y; };
  virtual int Width() { return box.width; };
  virtual int Height() { return box.height; };
  virtual SCM Direction() { return SCM_BOOL_F; };
  virtual int Borderwidth() { return box.borderwidth; };
  virtual int Shaped() { return box.shaped(); };
  virtual Decoration* MaybeMenu() { return this; };
  virtual void assure_free_deco(const char* tag, int pos);
  virtual int check_free_menu();
  Decoration* Parent() { return parent; };
  Decoration* Top() { Decoration* d=this; for(;d->parent;d=d->parent); return d; };
  ClientWindow* Win() { return window; };
  ScreenContext* Screen() { return screen; };
  class Fsm* GetFsm() { return fsm; };
  Box* GetBox() { return &box; };
  int Type() { return status; };
  int Valid() { return valid; };
  int Hidden() { return hidden; };
  int Visible();
  virtual void exec_recursive_event(XEvent* ev, int noreconf);
  virtual void issue_map_event();
  virtual void issue_unmap_event();
  void issue_resize_event();
  void issue_move_event();
  void issue_stack_event();
  int check_issue_stack_event();
  void issue_enter_recursive(Decoration* cp, XCrossingEvent* ev);
  void issue_enter_event(XCrossingEvent* ev);
  void issue_leave_event(XCrossingEvent* ev);
  virtual SCM scm() { return self; };
protected:
  void reset_shape();
  void paint_shape();
  virtual void paint_shape_intern(Window work, XRectangle* r);
  virtual void inherit_holes(Window hook, int xoff, int yoff);
  void inherit_shape(Window h);
  int compile_acord(SCM ac, float& out, float& in, int& off);
  void compile_anchor(SCM anchor, Anchor* descr, const char* func);
  void interpret_anchor(Anchor* descr, Box* b1, Box* b2, int& x, int& y, int& w, int& h);
  virtual void disable_parts() {};
  virtual void enable_parts() {};
  virtual void changed_stacking(Decoration* child) {};
  void CreateWindow(Window par);
  void MoveWindow();
  void ResizeWindow();
  void MoveResizeWindow();
  void SetWindowBorderwidth();
  SCM self;
  SCM bordercolor;
  SCM background;
  SCM behavior;
  SCM cursor;
  SCM anchor;
  SCM property;
  class Fsm* fsm;
  Box box;
  Box oldbox;
  int direction;
  int borderwidth;
  int min_width, max_width;
  int min_height, max_height;
  int width, height;
  int separator;
  int margin;
  int floating;
  Anchor* adescr;

  int valid;
  int status;
  int hidden;
  int has_client;
  int shape_dirty;
  Window hook;
  Decoration* parent;
  ClientWindow* window;
  ScreenContext* screen;
  int eflags;
};

class Bar : public Decoration {
public:
  Bar(SCM sub, SCM ctx, int cn, const char* tag);
  virtual ~Bar();
  virtual void mark();
  virtual int print(SCM port);
  virtual int open(int stat, class ClientWindow* cw, class ScreenContext* scr);
  virtual void semiopen(class ClientWindow* cw, class ScreenContext* scr);
  virtual void close(int flag);
  virtual void setnaturalsize(short pdir);
  virtual void updatesize(int xs, int ys);
  virtual void EventHandler(XEvent* evt);
  virtual void ReconfigureUp(int size, int pos, int shape, int inhibsh, int inhibrc);
  virtual void ReconfigureDown();
  virtual SCM get_parts();
  virtual SCM get_part(int n);
  virtual int get_num_parts();
  virtual void set_part(SCM obj, int n);
  virtual void insert_part(SCM obj, int n);
  virtual void remove_part(int n);
  virtual void remove_part(Decoration* part);
  virtual void reorder_part(int n, int m);
  void maintain_stacking(int n);
  virtual SCM Direction() { return (eflags & 1 ? WA_horizontal : eflags & 2 ? WA_vertical : WA_center); };
  virtual void exec_recursive_event(XEvent* ev, int noreconf);
  virtual void issue_map_event();
  virtual void issue_unmap_event();
protected:
  virtual void paint_shape_intern(Window work, XRectangle* r);
  virtual void inherit_holes(Window hook, int xoff, int yoff);
  virtual void disable_parts();
  virtual void enable_parts();
  virtual void changed_stacking(Decoration* child);
  int delay_reconfigure;
  int nplugs;
  Decoration** plugs;
};

class Plug : public Decoration {
public:
  Plug(SCM sub);
  virtual ~Plug();
  virtual void mark();
  virtual int print(SCM port);
  virtual int open(int stat, class ClientWindow* cw, class ScreenContext* scr);
  virtual void semiopen(class ClientWindow* cw, class ScreenContext* scr);
  virtual void setnaturalsize(short pdir);
  virtual void updatesize(int xs, int ys);
  virtual void EventHandler(XEvent* evt);
  virtual void ReconfigureDown();
  virtual void ReconfigureUp(int size, int pos, int shape, int inhibsh, int inhibrc);
  virtual SCM scm() { return graphic; };
  void set_graphic(SCM gr);
protected:
  virtual void paint_shape_intern(Window work, XRectangle* r);
  virtual void inherit_holes(Window hook, int xoff, int yoff);
  SCM graphic;
};

class InnerDeco : public Decoration {
public:
  InnerDeco() : Decoration() { inhibit_gravity = 0; inner_pos_dirty = 0; orig_x = 0; orig_y = 0; inner_x = 0; inner_y = 0; };
  virtual void GetInnerDims(int& ulx, int& uly, int& lrx, int& lry) = 0;
  int Xorig() { return orig_x; };
  int Yorig() { return orig_y; };
  void SetOrigPos(int x, int y) { orig_x = x; orig_y = y; };
  int OriginalBorderwidth() { return (orig_borderwidth == -1 ? 0 : orig_borderwidth); };
  int InhibitGravity() { return inhibit_gravity; };
protected:
  int orig_x, orig_y;
  int inner_x, inner_y;
  int inner_pos_dirty;
  int orig_borderwidth;
  int inhibit_gravity;
};

class IMenu : public InnerDeco {
public:
  IMenu(Decoration* mn, ScreenContext* scr);
  virtual ~IMenu();
  virtual void mark();
  void initialize(class ClientWindow* cw) { window = cw; orig_borderwidth = menu->borderwidth; };
  Decoration* unrealize();
  void pop(int x, int y, Decoration* par, class WlCursor* cursor, int grab_kbd, int grab_cld, int grab_cnf, int grab_nofrz);
  void unpop();
  virtual int print(SCM port);
  virtual SCM inspect(SCM ctxlst) { return menu->inspect(ctxlst); };
  virtual void modify(SCM ctx, int cn, const char* tag) { menu->modify(ctx, cn, tag); };
  virtual int open(int stat, class ClientWindow* cw, class ScreenContext* scr);
  virtual void close(int flag);
  virtual void updatesize(int xs, int ys);
  virtual void EventHandler(XEvent* evt);
  virtual void ReconfigureDown();
  virtual void ReconfigureTurn(int inhibrc);
  virtual void ReconfigureUp(int size, int pos, int shape, int inhibsh, int inhibrc);
  virtual void hide();
  virtual void show();
  virtual SCM get_bordercolor() { return menu->bordercolor; };
  virtual int get_borderwidth() { return menu->borderwidth; };
  virtual SCM get_background() { return menu->background; };
  virtual SCM get_cursor() { return menu->cursor; };
  virtual SCM get_anchor() { return menu->anchor; };
  virtual SCM get_property() { return menu->property; };
  virtual SCM get_behavior() { return menu->behavior; };
  virtual SCM get_parts() { return menu->get_parts(); };
  virtual SCM get_part(int n) { return menu->get_part(n); };
  virtual int get_num_parts() { return menu->get_num_parts(); };
  virtual void set_bordercolor(SCM o) { menu->set_bordercolor(o); };
  virtual void set_borderwidth(SCM wdt);
  virtual void set_background(SCM o) { menu->set_background(o); };
  virtual void set_cursor(SCM o) { menu->set_cursor(o); };
  virtual void set_anchor(SCM o) { menu->set_anchor(o); };
  virtual void set_property(SCM o) { menu->set_property(o); };
  virtual void set_behavior(SCM o) { menu->set_behavior(o); };
  virtual void set_part(SCM o, int n) { menu->set_part(o, n); };
  virtual void insert_part(SCM o, int n) { menu->insert_part(o, n); };
  virtual void remove_part(int n) { menu->remove_part(n); };
  virtual void reorder_part(int n, int m) { menu->reorder_part(n, m); };
  virtual void GetInnerDims(int& ulx, int& uly, int& lrx, int& lry);
  virtual int Width() { return menu->Width(); };
  virtual int Height() { return menu->Height(); };
  virtual SCM Direction() { return menu->Direction(); };
  virtual int Borderwidth() { return menu->Borderwidth(); };
  virtual void intern_set_borderwidth(int wdt);
  virtual Decoration* MaybeMenu() { return menu; };
  virtual void assure_free_deco(const char* tag, int pos);
  virtual int check_free_menu();
  virtual void exec_recursive_event(XEvent* ev, int noreconf);
  virtual void issue_map_event();
  virtual void issue_unmap_event();
  SCM DecoProcedure() { return decoproc; };
  void SetDecoProcedure(SCM proc) { decoproc = proc; };
  SCM IconProcedure() { return iconproc; };
  void SetIconProcedure(SCM proc) { iconproc = proc; };
protected:
  void internal_shape();
  Decoration* menu;
  SCM decoproc;
  SCM iconproc;
};

class IClient : public InnerDeco {
public:
  IClient(Window win, ClientWindow* cw, ScreenContext* scr);
  virtual int print(SCM port);
  virtual int open(int stat, class ClientWindow* cw, class ScreenContext* scr);
  virtual void close(int flag);
  virtual void updatesize(int xs, int ys);
  virtual void EventHandler(XEvent* evt);
  virtual void ReconfigureDown();
  virtual void ReconfigureUp(int size, int pos, int shape, int inhibsh, int inhibrc);
  virtual void set_bordercolor(SCM o) { bordercolor = o; }; // never used?
  virtual void set_background(SCM o) { background = o; };   // never used?
  virtual void intern_set_borderwidth(int wdt);
  void Resize(int w, int h);
  void MoveResize(int x, int y, int w, int h);
  void ReconfBorderwidth(int bw);
  void ConsiderClientSize(struct CachedProperties* props, int new_win);
  void SendSyntheticMoveEvent(int x, int y);
  virtual void GetInnerDims(int& ulx, int& uly, int& lrx, int& lry);
  Window XClient() { return wind; };
  virtual int Width() { return box.width - 2*inner_borderwidth; };
  virtual int Height() { return box.height - 2*inner_borderwidth; };
  virtual int Borderwidth() { return inner_borderwidth; };
  virtual int Shaped() { return box.hole_parts(); };
  void change_shape(int s);
protected:
  void internal_shape();
  virtual void inherit_holes(Window hook, int xoff, int yoff);
  Window wind;
  int inner_borderwidth;
};

class UScreen : public Decoration {
public:
  UScreen(ScreenContext* scr);
  virtual int print(SCM port);
  virtual int open(int stat, class ClientWindow* cw, class ScreenContext* scr);
  virtual void close(int flag);
  virtual void EventHandler(XEvent* evt);
  void ResizeScreen(int w, int h);
protected:
  int number;
};

Decoration* empty_deco();
Decoration* CommonParent(Decoration* d1, Decoration* d2);
int IsAnAncestor(class Decoration* parent, class Decoration* child);
void DoResizeClients();

#define AUTODIR  		0
#define HORIZONTAL		1
#define VERTICAL		2
#define CENTER    		3
#define SINGLE    		4

#define NoStatus 0
#define AnyStatus 7
#define WindowStatus 1
#define IconStatus 2
#define ScreenStatus 4
#define ClientStatus 8
#define MenuStatus 16
#define InternMenuStatus 32

#define WLDECOP(x) (SCM_NIMP(x) && SCM_CAR(x) == (SCM)scm_tc16_deco)
#define WL_DECO(x) ((Decoration*) SCM_CDR(x))
#define WLWINDOWP(x) (SCM_NIMP(x) && SCM_CAR(x) == (SCM)scm_tc16_deco && ((Decoration*) SCM_CDR(x))->Win())
#define WL_WINDOW(x) ((ClientWindow*) ((Decoration*) SCM_CDR(x))->Win())
#define WLSCRWINP(x) (SCM_NIMP(x) && SCM_CAR(x) == (SCM)scm_tc16_deco && ((Decoration*) SCM_CDR(x))->Screen())
#define WLSCREENP(x) (SCM_NIMP(x) && SCM_CAR(x) == (SCM)scm_tc16_deco && ((Decoration*) SCM_CDR(x))->Screen() && !((Decoration*) SCM_CDR(x))->Win())
#define WL_SCREEN(x) ((ScreenContext*) ((Decoration*) SCM_CDR(x))->Screen())
