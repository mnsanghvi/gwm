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
 * Decoration Object   *
 * 		       *
 \*********************/

#include <guile/gh.h>
#include <stdio.h>
#include "gwm.hh"
#include "gwmfunc.hh"
#include "error.hh"
#include "paint.hh"
#include "active.hh"
#include "cursor.hh"
#include "deco.hh"
#include "event.hh"
#include "fsm.hh"
#include "screen.hh"
#include "client.hh"
#include "drawing.hh"

/*
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
*/ 

extern FILE* gwmlog;   // log file used for debugging

#define GWMLOG(str, obj) if(gwmlog) { fprintf(gwmlog, str, obj); fflush(gwmlog); }

long scm_tc16_deco;

SCM mark_deco(SCM obj)
{
  if (WL_DECO(obj))
    WL_DECO(obj)->mark();
  return SCM_BOOL_F;
}

void Decoration::mark()
{
  if (background) scm_gc_mark(background);
  if (bordercolor) scm_gc_mark(bordercolor);
  if (behavior) scm_gc_mark(behavior);
  if (cursor) scm_gc_mark(cursor);
  if (anchor) scm_gc_mark(anchor);
  if (property) scm_gc_mark(property);
  if (parent && parent->self)
    scm_gc_mark(parent->self);
}

void Bar::mark()
{
  int i;
  Decoration::mark();
  for (i=0; i<nplugs; i++)
    if (plugs[i] && plugs[i]->self)
      scm_gc_mark(plugs[i]->self);
}

void Plug::mark()
{
  //  Decoration::mark();
  if (graphic) scm_gc_mark(graphic);
}

void IMenu::mark()
{
  Decoration::mark();
  menu->mark();
  if (decoproc) scm_gc_mark(decoproc);
  if (iconproc) scm_gc_mark(iconproc);
}

struct DelayedResizeClient {
  DelayedResizeClient(IClient* cw, int ww, int hh) { cwin = cw; nw = ww; nh = hh; ow = cw->Width(); oh = cw->Height(); next = 0; };
  IClient* cwin;
  int nw, nh, ow, oh;
  DelayedResizeClient* next;
};

static DelayedResizeClient* delayed_resize_list = 0;

void DelayResizeClient(IClient* cw, int ww, int hh)
{
  DelayedResizeClient* rc = delayed_resize_list;
  while (rc && rc->cwin != cw)
    rc = rc->next;
  if (!rc) {
    rc = new DelayedResizeClient(cw, ww, hh);
    rc->next = delayed_resize_list;
    delayed_resize_list = rc;
  } else {
    rc->nw = ww;
    rc->nh = hh;
  }
}

void DoResizeClients()
{
  DelayedResizeClient* rc;
  while (delayed_resize_list) {
    rc = delayed_resize_list;
    delayed_resize_list = delayed_resize_list->next;
    if (rc->cwin->Valid() > 0)
      XResizeWindow(dpy, rc->cwin->XClient(), rc->nw, rc->nh);
    delete rc;
  }
}

int DelayedClientSize(IClient* cw, int& w, int& h)
{
  DelayedResizeClient* rc = delayed_resize_list;
  while (rc && rc->cwin != cw)
    rc = rc->next;
  if (rc) {
    w = rc->ow;
    h = rc->oh;
    return 1;
  } else
    return 0;
}

int IsAnAncestor(Decoration* parent, Decoration* wob)
{
  for (; wob && wob != parent; wob = wob->Parent());
  return (wob ? TRUE : FALSE);
}

Decoration* CommonParent(Decoration* d1, Decoration* d2)
{
  int i, j;
  Decoration* tmp;
  if (d1 == d2->Parent())
    return d1;
  for (tmp=d1, i=0; tmp; i++, tmp=tmp->Parent());
  for (tmp=d2, j=0; tmp; j++, tmp=tmp->Parent());
  for (;i<j; j--, d2=d2->Parent());
  for (;i>j; i--, d1=d1->Parent());
  for (; d1 != d2; d1=d1->Parent(), d2=d2->Parent());
  return d1;
}

struct DelayedReconfigureItem {
  DelayedReconfigureItem(Decoration* w) { wob = w; next = 0; };
  Decoration *wob;
  DelayedReconfigureItem* next;
};

static DelayedReconfigureItem* delay_reconfigure_list = 0;

void DelayReconfigure(Decoration* w)
{
  DelayedReconfigureItem *item1, *item2 = delay_reconfigure_list;
  while (item2) {
    if (IsAnAncestor(item2->wob, w) && (w->Type() & MenuStatus) == (item2->wob->Type() & MenuStatus))
      return;
    item2 = item2->next;
  }
  item1 = new DelayedReconfigureItem(w);
  item1->next = delay_reconfigure_list;
  delay_reconfigure_list = item1;
  while (item1->next)
    if (IsAnAncestor(w, item1->next->wob)) {
      if ((w->Type() & MenuStatus) == (item1->next->wob->Type() & MenuStatus)) {
        item2 = item1->next;
        item1->next = item2->next;
        delete item2;
      } else { // May this happen only once...
        delay_reconfigure_list->wob = item1->next->wob;
        item1->next->wob = w;
        item1 = item1->next;
      }
    } else
      item1 = item1->next;
}

void ForceReconfigure()
{
  DelayedReconfigureItem *item1, *item2 = delay_reconfigure_list;
  int rubber = GWM_rubber_feedback && delay_reconfigure_list;
  if (rubber) {
    UnDrawRubber();
    GWM_rubber_feedback = 0;
  }
  while (item2) {
    item2->wob->ReconfigureDown();
    item1 = item2->next;
    delete item2;
    item2 = item1;
  }
  if (rubber) {
    ReDrawRubber();
    GWM_rubber_feedback = 1;
  }
  delay_reconfigure_list = 0;
}

size_t free_deco(SCM obj)
{
  if (WL_DECO(obj))
    delete WL_DECO(obj);
  return 0;
}

int print_deco(SCM obj, SCM port, scm_print_state * pstate)
{
  return WL_DECO(obj)->print(port);
}

int Bar::print(SCM port)
{
  int             i;
  scm_puts("#<deco: ", port);
  for (i = 0; i < nplugs; i++) {
    if (i) scm_puts(", ", port);
    if (plugs[i])
      scm_write(plugs[i]->scm(), port);
    else
      scm_puts("()", port);
  }
  scm_puts(">", port);
  return 1;
}

int Plug::print(SCM port)
{
  scm_write(graphic, port);
  return 1;
}

int IMenu::print(SCM port)
{
  menu->print(port);
  return 1;
}

int IClient::print(SCM port)
{
  scm_puts("#<client ", port);
  scm_write(gh_long2scm(wind), port);
  scm_puts(">", port);
  return 1;
}

int UScreen::print(SCM port)
{
  scm_puts("#<screen ", port);
  scm_write(gh_long2scm(number), port);
  scm_puts(">", port);
  return 1;
}


SCM deco2scm(Decoration* deco)
{
  return scm_cell((scm_t_bits) scm_tc16_deco, (scm_t_bits) deco);
}

Decoration* empty_deco()
{
  return new Bar(SCM_EOL, SCM_EOL, 0, 0);
}

Decoration::Decoration(SCM ctx, int cn, const char* tag)
{
  SCM tmp;
  tmp = gwm_get_keyword(k_direction, ctx, cn, SCM_BOOL_F);
  if (tmp == SCM_BOOL_F)
    direction = AUTODIR;
  else if (tmp == WA_horizontal)
    direction = HORIZONTAL;
  else if (tmp == WA_vertical)
    direction = VERTICAL;
  else if (tmp == WA_center)
    direction = CENTER;
  else 
    gwm_wrong_type_arg(tag, 0, tmp, "'horizontal, 'vertical or 'center");
  width = wl_getposint(gwm_get_keyword(k_width, ctx, cn, SCM_BOOL_F), tag);
  height = wl_getposint(gwm_get_keyword(k_height, ctx, cn, SCM_BOOL_F), tag);
  min_width = wl_getposint(gwm_get_keyword(k_min_width, ctx, cn, SCM_BOOL_F), tag);
  max_width = wl_getposint(gwm_get_keyword(k_max_width, ctx, cn, SCM_BOOL_F), tag);
  min_height = wl_getposint(gwm_get_keyword(k_min_height, ctx, cn, SCM_BOOL_F), tag);
  max_height = wl_getposint(gwm_get_keyword(k_max_height, ctx, cn, SCM_BOOL_F), tag);
  separator = wl_getposint(gwm_get_keyword(k_separator, ctx, cn, SCM_BOOL_F), tag);
  if (separator == -1) separator = 0;
  margin = wl_getposint(gwm_get_keyword(k_margin, ctx, cn, SCM_BOOL_F), tag);
  if (margin == -1) margin = 0;
  borderwidth = wl_getposint(gwm_get_keyword(k_borderwidth, ctx, cn, SCM_BOOL_F), tag);
  bordercolor = gwm_get_keyword(k_bordercolor, ctx, cn, SCM_UNDEFINED);
  if (bordercolor == SCM_UNDEFINED) bordercolor = Context->pixel.Black;
  else if (!WLPAINTP(bordercolor))
    gwm_wrong_type_arg(tag, 0, bordercolor, "color or pixmap");
  background = gwm_get_keyword(k_background, ctx, cn, SCM_UNDEFINED);
  if (background == SCM_UNDEFINED) background = Context->pixel.White;
  else if (!WLPAINTP(background))
    gwm_wrong_type_arg(tag, 0, background, "color or pixmap");
  behavior = gwm_get_keyword(k_behavior, ctx, cn, SCM_BOOL_F);
  if (behavior != SCM_BOOL_F && !WLBEHAVIORP(behavior))
    gwm_wrong_type_arg(tag, 0, behavior, "behavior");
  cursor = gwm_get_keyword(k_cursor, ctx, cn, SCM_BOOL_F);
  if (cursor != SCM_BOOL_F && !WLCURSORP(cursor))
    gwm_wrong_type_arg(tag, 0, cursor, "cursor");
  anchor = gwm_get_keyword(k_anchor, ctx, cn, SCM_BOOL_F);
  if (anchor == SCM_BOOL_F) anchor = SCM_EOL;
  else if (!gh_list_p(anchor))
    gwm_wrong_type_arg(tag, 0, anchor, "list");
  property = gwm_get_keyword(k_property, ctx, cn, SCM_EOL);
  if (!gh_list_p(property))
    gwm_wrong_type_arg(tag, 0, property, "list");
  fsm = 0;
  if (anchor != SCM_EOL) {
    compile_anchor(anchor, (adescr = new Anchor()), tag);
    floating = 1;
  } else {
    adescr = 0;
    floating = 0;
  }
  if (WLPIXMAPP(background)) WL_PIXMAP(background)->register_use(this, 0);
  if (WLPIXMAPP(bordercolor)) WL_PIXMAP(bordercolor)->register_use(this, 1);
  eflags = 0;
  has_client = 0;
  shape_dirty = 0;
  valid = 0;
  status = NoStatus;
  hidden = 0;
  parent = 0;
  screen = 0;
  window = 0;
  self = deco2scm(this);
}

Decoration::Decoration()
{
  direction = SINGLE;
  width = -1;
  height = -1;
  min_width = -1;
  max_width = -1;
  min_height = -1;
  max_height = -1;
  separator = 0;
  margin = 0;
  borderwidth = -1;
  bordercolor = SCM_BOOL_F;
  background = SCM_BOOL_F;
  behavior = SCM_BOOL_F;
  cursor = SCM_BOOL_F;
  anchor = SCM_EOL;
  property = SCM_EOL;
  fsm = 0;
  adescr = 0;
  floating = 0;
  eflags = 0;
  has_client = 0;
  shape_dirty = 0;
  valid = 0;
  status = NoStatus;
  hidden = 0;
  parent = 0;
  screen = 0;
  window = 0;
  self = 0;
}

Decoration::~Decoration()
{
  if (WLPIXMAPP(background)) WL_PIXMAP(background)->unregister_use(this, 0);
  if (WLPIXMAPP(bordercolor)) WL_PIXMAP(bordercolor)->unregister_use(this, 1);
  if (adescr)
    delete adescr;
}

SCM Decoration::inspect(SCM ctxlst)
{
  SCM ctx = SCM_EOL;
  if (ctxlst == SCM_EOL || scm_memq(k_property, ctxlst) != SCM_BOOL_F)
    ctx = scm_cons(k_property, scm_cons(property, ctx));
  if ((ctxlst == SCM_EOL && cursor != SCM_BOOL_F) ||
      scm_memq(k_cursor, ctxlst) != SCM_BOOL_F)
    ctx = scm_cons(k_cursor, scm_cons(cursor, ctx));
  if ((ctxlst == SCM_EOL && behavior != SCM_BOOL_F) ||
      scm_memq(k_behavior, ctxlst) != SCM_BOOL_F)
    ctx = scm_cons(k_behavior, scm_cons(behavior, ctx));
  if ((ctxlst == SCM_EOL && anchor != SCM_BOOL_F) ||
      scm_memq(k_anchor, ctxlst) != SCM_BOOL_F)
    ctx = scm_cons(k_anchor, scm_cons(anchor == SCM_EOL ? SCM_BOOL_F : anchor,
                                      ctx));
  if ((ctxlst == SCM_EOL && background != SCM_BOOL_F) ||
      scm_memq(k_background, ctxlst) != SCM_BOOL_F)
    ctx = scm_cons(k_background, scm_cons(background, ctx));
  if ((ctxlst == SCM_EOL && bordercolor != SCM_BOOL_F) ||
      scm_memq(k_bordercolor, ctxlst) != SCM_BOOL_F)
    ctx = scm_cons(k_bordercolor, scm_cons(bordercolor, ctx));
  if ((ctxlst == SCM_EOL && borderwidth != -1) ||
      scm_memq(k_borderwidth, ctxlst) != SCM_BOOL_F)
    ctx = scm_cons(k_borderwidth,
                   scm_cons(borderwidth==-1 ? SCM_BOOL_F : gh_int2scm(borderwidth),
                            ctx));
  if ((ctxlst == SCM_EOL && margin != 0) ||
      scm_memq(k_margin, ctxlst) != SCM_BOOL_F)
    ctx = scm_cons(k_margin, scm_cons(gh_int2scm(margin), ctx));
  if ((ctxlst == SCM_EOL && separator != 0) ||
      scm_memq(k_separator, ctxlst) != SCM_BOOL_F)
    ctx = scm_cons(k_separator, scm_cons(gh_int2scm(separator), ctx));
  if ((ctxlst == SCM_EOL && max_width != -1) ||
      scm_memq(k_max_width, ctxlst) != SCM_BOOL_F)
    ctx = scm_cons(k_max_width,
                   scm_cons(max_width==-1 ? SCM_BOOL_F : gh_int2scm(max_width),
                            ctx));
  if ((ctxlst == SCM_EOL && min_width != -1) ||
      scm_memq(k_min_width, ctxlst) != SCM_BOOL_F)
    ctx = scm_cons(k_min_width,
                   scm_cons(min_width==-1 ? SCM_BOOL_F : gh_int2scm(min_width),
                            ctx));
  if ((ctxlst == SCM_EOL && max_height != -1) ||
      scm_memq(k_max_height, ctxlst) != SCM_BOOL_F)
    ctx = scm_cons(k_max_height,
                   scm_cons(max_height==-1 ? SCM_BOOL_F : gh_int2scm(max_height),
                            ctx));
  if ((ctxlst == SCM_EOL && min_height != -1) ||
      scm_memq(k_min_height, ctxlst) != SCM_BOOL_F)
    ctx = scm_cons(k_min_height,
                   scm_cons(min_height==-1 ? SCM_BOOL_F : gh_int2scm(min_height),
                            ctx));
  if ((ctxlst == SCM_EOL && height != -1) ||
      scm_memq(k_height, ctxlst) != SCM_BOOL_F)
    ctx = scm_cons(k_height,
                   scm_cons(height==-1 ? SCM_BOOL_F : gh_int2scm(height), ctx));
  if ((ctxlst == SCM_EOL && width != -1) ||
      scm_memq(k_width, ctxlst) != SCM_BOOL_F)
    ctx = scm_cons(k_width,
                   scm_cons(width==-1 ? SCM_BOOL_F : gh_int2scm(width), ctx));
  if ((ctxlst == SCM_EOL && direction != AUTODIR && direction != SINGLE) ||
      scm_memq(k_direction, ctxlst) != SCM_BOOL_F)
    ctx = scm_cons(k_direction, scm_cons(direction == HORIZONTAL ? 
                                         WA_horizontal :
                                         direction == VERTICAL ?
                                         WA_vertical :
                                         direction == CENTER ?
                                         WA_center :
                                         SCM_BOOL_F, 
                                         ctx));
  return ctx;
}

void Decoration::modify(SCM ctx, int cn, const char* tag)
{
  SCM tmp;
  int old_sh, old_anc2, val, dp = 0, ds = 0, dsh = 0;
  int rubber = GWM_rubber_feedback && Visible();
  if (rubber) {
    UnDrawRubber();
    GWM_rubber_feedback = 0;
  }
  if ((tmp = gwm_get_keyword(k_direction, ctx, cn, SCM_UNDEFINED)) != SCM_UNDEFINED &&
      direction != SINGLE) {
    if (tmp == WA_horizontal) {
      if (direction != HORIZONTAL) ds = -1;
      direction = HORIZONTAL;
    } else if (tmp == WA_vertical) {
      if (direction != VERTICAL) ds = -1;
      direction = VERTICAL;
    } else if (tmp == WA_center) {
      if (direction != CENTER) ds = -1;
      direction = CENTER;
    } else if (tmp == SCM_BOOL_F) {
      if (direction != AUTODIR) ds = -1;
      direction = AUTODIR;
    } else 
      gwm_wrong_type_arg(tag, 0, tmp, "'horizontal, 'vertical or 'center");
  }
  if ((tmp = gwm_get_keyword(k_width, ctx, cn, SCM_UNDEFINED)) != SCM_UNDEFINED) {
    val = wl_getposint(tmp, tag);
    if (width != val) ds = -1;
    width = val;
  }
  if ((tmp = gwm_get_keyword(k_height, ctx, cn, SCM_UNDEFINED)) != SCM_UNDEFINED) {
    val = wl_getposint(tmp, tag);
    if (height != val) ds = -1;
    height = val;
  }
  if ((tmp = gwm_get_keyword(k_min_width, ctx, cn, SCM_UNDEFINED)) != SCM_UNDEFINED) {
    val = wl_getposint(tmp, tag);
    if (min_width != val) ds = -1;
    min_width = val;
  }
  if ((tmp = gwm_get_keyword(k_max_width, ctx, cn, SCM_UNDEFINED)) != SCM_UNDEFINED) {
    val = wl_getposint(tmp, tag);
    if (max_width != val) ds = -1;
    max_width = val;
  }
  if ((tmp = gwm_get_keyword(k_min_height, ctx, cn, SCM_UNDEFINED)) != SCM_UNDEFINED) {
    val = wl_getposint(tmp, tag);
    if (min_height != val) ds = -1;
    min_height = val;
  }
  if ((tmp = gwm_get_keyword(k_max_height, ctx, cn, SCM_UNDEFINED)) != SCM_UNDEFINED) {
    val = wl_getposint(tmp, tag);
    if (max_height != val) ds = -1;
    max_height = val;
  }
  if ((tmp = gwm_get_keyword(k_separator, ctx, cn, SCM_UNDEFINED)) != SCM_UNDEFINED) {
    separator = wl_getposint(tmp, tag);
    if (separator == -1) separator = 0;
    ds = -1;
  }
  if ((tmp = gwm_get_keyword(k_margin, ctx, cn, SCM_UNDEFINED)) != SCM_UNDEFINED) {
    margin = wl_getposint(tmp, tag);
    if (margin == -1) margin = 0;
    ds = -1;
  }
  if ((tmp = gwm_get_keyword(k_borderwidth, ctx, cn, SCM_UNDEFINED)) != SCM_UNDEFINED) {
    borderwidth = wl_getposint(tmp, tag);
    val = Borderwidth();
    if (status & InternMenuStatus)
      parent->intern_set_borderwidth(borderwidth);
    else
      intern_set_borderwidth(borderwidth);
    if (Borderwidth() != val) ds = -1;
  }
  if ((tmp = gwm_get_keyword(k_bordercolor, ctx, cn, SCM_UNDEFINED)) != SCM_UNDEFINED) {
    if (WLPAINTP(tmp)) {
      if (WLPIXMAPP(bordercolor)) WL_PIXMAP(bordercolor)->unregister_use(this, 1);
      bordercolor = tmp;
      if (WLPIXMAPP(bordercolor)) WL_PIXMAP(bordercolor)->register_use(this, 1);
      old_sh = box.shaped_border();
      box.set_trans_border(WL_PAINT(bordercolor)->IsShapedT());
      box.set_hole_border(WL_PAINT(bordercolor)->IsShapedH());
      if (valid > 0)
        WL_PAINT(bordercolor)->paint_border(hook, &box);
      if (old_sh || box.shaped_border())
        dsh=-1;
    } else if (tmp != SCM_BOOL_F) // just ignore
      gwm_wrong_type_arg(tag, 0, tmp, "color or pixmap");
  }
  if ((tmp = gwm_get_keyword(k_background, ctx, cn, SCM_UNDEFINED)) != SCM_UNDEFINED) {
    if (WLPAINTP(tmp)) {
      if (WLPIXMAPP(background)) WL_PIXMAP(background)->unregister_use(this, 0);
      background = tmp;
      if (WLPIXMAPP(background)) WL_PIXMAP(background)->register_use(this, 0);
      old_sh = box.shaped_back();
      box.set_trans_back(WL_PAINT(background)->IsShapedT());
      box.set_hole_back(WL_PAINT(background)->IsShapedH());
      if (valid > 0)
        WL_PAINT(background)->paint_background(hook, &box);
      if (old_sh || box.shaped_back())
        dsh=-1;
    } else if (tmp != SCM_BOOL_F) // just ignore
      gwm_wrong_type_arg(tag, 0, tmp, "color or pixmap");
  }
  if ((tmp = gwm_get_keyword(k_anchor, ctx, cn, SCM_UNDEFINED)) != SCM_UNDEFINED) {
    if (tmp == SCM_BOOL_F)
      tmp = SCM_EOL;
    else if (!gh_list_p(tmp))
      gwm_wrong_type_arg(tag, 0, tmp, "list");
    if (status & InternMenuStatus)
      ds |= (parent->intern_set_anchor(tmp) ? -1 : 0);
    else
      ds |= intern_set_anchor(tmp);
    dp = -1;
    anchor = tmp;
  }
  if ((tmp = gwm_get_keyword(k_behavior, ctx, cn, SCM_UNDEFINED)) != SCM_UNDEFINED) {
    if (tmp == SCM_BOOL_F || WLBEHAVIORP(tmp)) {
      behavior = tmp;
      if (valid != 0 && fsm)
        if (!WLBEHAVIORP(tmp))
          fsm->install_nullstate();
        else
          fsm->install_state(WL_BEHAVIOR(tmp));
    } else
      gwm_wrong_type_arg(tag, 0, tmp, "behavior");
  }
  if ((tmp = gwm_get_keyword(k_cursor, ctx, cn, SCM_UNDEFINED)) != SCM_UNDEFINED) {
    if (tmp == SCM_BOOL_F || WLCURSORP(tmp)) {
      cursor = tmp;
      if (valid > 0)
        if (WLCURSORP(tmp))
          WL_CURSOR(tmp)->use_cursor(hook);
        else
          WlCursor::use_no_cursor(hook);
    } else
      gwm_wrong_type_arg(tag, 0, tmp, "cursor");
  }
  if ((tmp = gwm_get_keyword(k_property, ctx, cn, SCM_UNDEFINED)) != SCM_UNDEFINED) {
    if (gh_list_p(tmp))
      property = tmp;
    else
      gwm_wrong_type_arg(tag, 0, tmp, "list");
  }
  if (ds || dsh || dp)
    ReconfigureUp(ds, dp, dsh, 0, 0);
  if (rubber) {
    ReDrawRubber();
    GWM_rubber_feedback = 1;
  }
}

void Decoration::dimensions(Box* b)
{
    b->x = box.x;
    b->y = box.y;
    b->width = box.width + 2 * box.borderwidth;
    b->height = box.height + 2 * box.borderwidth;
}

void Decoration::set_borderwidth(SCM wdt)
{
  borderwidth = (wdt == SCM_BOOL_F ? -1 : gh_scm2int(wdt));
  int rubber;
  rubber = GWM_rubber_feedback && Visible();
  if (rubber) {
    UnDrawRubber();
    GWM_rubber_feedback = 0;
  }
  intern_set_borderwidth(borderwidth);
  ReconfigureUp(-1, 0, 0, 0, 0);
  if (rubber) {
    ReDrawRubber();
    GWM_rubber_feedback = 1;
  }
}

void IMenu::set_borderwidth(SCM wdt)
{
  menu->borderwidth = (wdt == SCM_BOOL_F ? -1 : gh_scm2int(wdt));
  int rubber = GWM_rubber_feedback && Visible();
  if (rubber) {
    UnDrawRubber();
    GWM_rubber_feedback = 0;
  }
  intern_set_borderwidth(menu->borderwidth);
  ReconfigureUp(1, 0, 0, 0, 0);
  if (rubber) {
    ReDrawRubber();
    GWM_rubber_feedback = 1;
  }
}

void Decoration::intern_set_borderwidth(int wdt)
{
  box.borderwidth = (wdt < 0 ? 0 : wdt);
  if (valid != 0 && hook)
    SetWindowBorderwidth();
}

void IMenu::intern_set_borderwidth(int wdt)
{
  if (wdt == -1)
    wdt = orig_borderwidth;
  menu->intern_set_borderwidth(wdt);
}

void IClient::intern_set_borderwidth(int wdt)
{
  int ow = inner_borderwidth;
  if (wdt == -1)
    inner_borderwidth = orig_borderwidth;
  else
    inner_borderwidth = wdt;
  if (ow != inner_borderwidth) {
    box.width += 2*(inner_borderwidth - ow);
    box.height += 2*(inner_borderwidth - ow);
    XSetWindowBorderWidth(dpy, wind, inner_borderwidth);
    ResizeWindow();
    if (box.hole_parts())
      internal_shape();
  }
}

void Decoration::set_bordercolor(SCM obj)
{
  int old_sh = box.shaped_border(), rubber;
  if (WLPIXMAPP(bordercolor)) WL_PIXMAP(bordercolor)->unregister_use(this, 1);
  bordercolor = obj;
  if (WLPIXMAPP(bordercolor)) WL_PIXMAP(bordercolor)->register_use(this, 1);
  rubber = GWM_rubber_feedback && Visible();
  if (rubber) {
    UnDrawRubber();
    GWM_rubber_feedback = 0;
  }
  if (WLPAINTP(bordercolor)) {
    box.set_trans_border(WL_PAINT(bordercolor)->IsShapedT());
    box.set_hole_border(WL_PAINT(bordercolor)->IsShapedH());
    if (valid > 0)
      WL_PAINT(bordercolor)->paint_border(hook, &box);
  }
  if (old_sh || box.shaped_border())
    ReconfigureUp(0, 0, -1, 0, 0);
  if (rubber) {
    ReDrawRubber();
    GWM_rubber_feedback = 1;
  }
}

void Decoration::set_background(SCM obj)
{
  int old_sh = box.shaped_back(), rubber;
  if (WLPIXMAPP(background)) WL_PIXMAP(background)->unregister_use(this, 0);
  background = obj;
  if (WLPIXMAPP(background)) WL_PIXMAP(background)->register_use(this, 0);
  rubber = GWM_rubber_feedback && Visible();
  if (rubber) {
    UnDrawRubber();
    GWM_rubber_feedback = 0;
  }
  if (WLPAINTP(background)) {
    box.set_trans_back(WL_PAINT(background)->IsShapedT());
    box.set_hole_back(WL_PAINT(background)->IsShapedH());
    if (valid > 0)
      WL_PAINT(background)->paint_background(hook, &box);
  }
  if (old_sh || box.shaped_back())
    ReconfigureUp(0, 0, -1, 0, 0);
  if (rubber) {
    ReDrawRubber();
    GWM_rubber_feedback = 1;
  }
}

void Decoration::set_cursor(SCM obj)
{
  cursor = obj;
  if (valid > 0) {
    if (WLCURSORP(obj))
      WL_CURSOR(obj)->use_cursor(hook);
    else
      WlCursor::use_no_cursor(hook);
  }
}

void Decoration::set_anchor(SCM obj)
{
  int old_anc2, rubber, sz = 0;
  if (obj == SCM_BOOL_F)
    obj = SCM_EOL;
  if (status & InternMenuStatus)
    sz = (parent->intern_set_anchor(obj) ? -1 : 0);
  else
    sz = intern_set_anchor(obj);
  anchor = obj;
  rubber = GWM_rubber_feedback && Visible();
  if (rubber) {
    UnDrawRubber();
    GWM_rubber_feedback = 0;
  }
  ReconfigureUp(sz, -1, 0, 0, 0);
  if (rubber) {
    ReDrawRubber();
    GWM_rubber_feedback = 1;
  }
}

int Decoration::intern_set_anchor(SCM obj)
{
  int sz = 0;
  int old_anc2;
  if (floating || obj != SCM_EOL) {
    old_anc2 = (adescr && (adescr->numx == 2 || adescr->numy == 2));
    compile_anchor(obj, (adescr ? adescr : adescr = new Anchor()), 0);
    if ((!floating) == (obj != SCM_EOL) || old_anc2 || adescr->numx == 2 || adescr->numy == 2)
      sz = -1;
    else if (parent) {
      int x, y, w, h;
      interpret_anchor(adescr, &parent->box, &box, x, y, w, h);
      updatepos(x, y);
      sz = 1; // new inner pos
    }
    floating = (obj != SCM_EOL);
    if (valid > 0 && parent)
      parent->changed_stacking(this);
  }
  return sz;
}

void Decoration::set_property(SCM obj)
{
  property = obj;
}

void Decoration::set_behavior(SCM obj)
{
  behavior = obj;
  if (valid != 0 && fsm) {
    if (!WLBEHAVIORP(obj))
      fsm->install_nullstate();
    else
      fsm->install_state(WL_BEHAVIOR(obj));
  }
}

void Decoration::hide()
{
  int rubber, vis, ohid = hidden;
  if (parent)
    hidden |= 1;
  else
    hidden = 1;
  if (valid > 0) {
    if (!parent) {
      window->MaybeMapUnmap();
    } else if (!ohid && hidden == 1) {
      vis = Visible();
      rubber = (vis && GWM_rubber_feedback);
      if (rubber) {
        UnDrawRubber();
        GWM_rubber_feedback = 0;
      }
      if (vis) {
        CatchUnmapRelease(this);
        issue_unmap_event();
      }
      XUnmapWindow(dpy, hook);
      ReconfigureUp(0, -1, box.shaped() ? -1 : 0, 0, 0);
      if (rubber) {
        ReDrawRubber();
        GWM_rubber_feedback = 1;
      }
    }
  } else if (box.shaped())
    ReconfigureUp(0, 0, -1, 0, 0);
}

void IMenu::hide()
{
  int rubber, vis, ohid = hidden;
  if (parent)
    hidden |= 1;
  else
    hidden = 1;
  if (valid > 0) {
    if (!parent) {
      window->MaybeMapUnmap();
    } else if (!ohid && hidden == 1) {
      vis = Visible();
      rubber = (vis && GWM_rubber_feedback);
      if (rubber) {
        UnDrawRubber();
        GWM_rubber_feedback = 0;
      }
      if (vis) {
        CatchUnmapRelease(this);
        issue_unmap_event();
      }
      XUnmapWindow(dpy, hook);
      menu->ReconfigureUp(0, -1, box.shaped() ? -1 : 0, 0, 0);
      if (rubber) {
        ReDrawRubber();
        GWM_rubber_feedback = 1;
      }
    }
  } else if (box.shaped())
    menu->ReconfigureUp(0, 0, -1, 0, 0);
}

void Decoration::show()
{
  int rubber, vis, ohid = hidden;
  if (parent)
    hidden &= ~1;
  else
    hidden = 0;
  if (valid > 0) {
    if (!parent) {
      window->MaybeMapUnmap();
    } else if (ohid && !hidden) {
      vis = Visible();
      rubber = (vis && GWM_rubber_feedback);
      if (rubber) {
        UnDrawRubber();
        GWM_rubber_feedback = 0;
      }
      if (vis) 
        issue_map_event();
      XMapWindow(dpy, hook);
      ReconfigureUp(0, -1, box.shaped() ? -1 : 0, 0, 0);
      if (rubber) {
        ReDrawRubber();
        GWM_rubber_feedback = 1;
      }
    }
  } else if (box.shaped())
    ReconfigureUp(0, 0, -1, 0, 0);
}

void IMenu::show()
{
  int rubber, vis, ohid = hidden;
  if (parent)
    hidden &= ~1;
  else
    hidden = 0;
  if (valid > 0) {
    if (!parent) {
      window->MaybeMapUnmap();
    } else if (ohid && !hidden) {
      vis = Visible();
      rubber = (vis && GWM_rubber_feedback);
      if (rubber) {
        UnDrawRubber();
        GWM_rubber_feedback = 0;
      }
      if (vis) 
        issue_map_event();
      XMapWindow(dpy, hook);
      menu->ReconfigureUp(0, -1, box.shaped() ? -1 : 0, 0, 0);
      if (rubber) {
        ReDrawRubber();
        GWM_rubber_feedback = 1;
      }
    }
  } else if (box.shaped())
    menu->ReconfigureUp(0, 0, -1, 0, 0);
}

void Decoration::force_show()
{
  if (status & IconStatus)
    window->Icon()->hidden = -1;
  else
    window->Deco()->hidden = -1;
  window->MaybeMapUnmap();
}

void Decoration::unforce_show()
{
  if (status & IconStatus)
    window->Icon()->hidden = 0;
  else
    window->Deco()->hidden = 0;
  window->MaybeMapUnmap();
}

int Decoration::Visible()
{
  Decoration* tmp;
  if (valid <= 0)
    return 0;
  if (window && (status & IconStatus ? !window->MappedIcon() : !window->MappedWin()))
    return 0;
  for(tmp=this; tmp && !tmp->hidden; tmp=tmp->parent);
  return !tmp;
}

void Decoration::assure_free_deco(const char* tag, int pos)
{
  if (parent)
    gwm_misc_error(tag, "Deco already has a parent: ~A", self);
  if (valid > 0)
    gwm_wrong_type_arg(tag, pos, self, "unopened deco");
}

void IMenu::assure_free_deco(const char* tag, int pos)
{
  if (valid > 0)
    gwm_wrong_type_arg(tag, pos, self, "unopened deco");
}

int Decoration::check_free_menu()
{
  return 0;
}

int IMenu::check_free_menu()
{
  return (window == 0);
}

Bar::Bar(SCM sub, SCM ctx, int cn, const char* tag)
  : Decoration(ctx, cn, tag)
{
  int i;
  SCM obj, arg;
  int ntabs = 0;
  delay_reconfigure = 0;
  nplugs = 0;
  plugs = 0;
  for (i=0, arg=sub; SCM_NNULLP(arg); i++, arg=SCM_CDR(arg)) {
    obj = SCM_CAR(arg);
    if (obj == SCM_EOL || WLPIXMAPP(obj) || WLACTIVEP(obj)) {
      nplugs++;
    } else if (obj == SCM_BOOL_F) {
    } else if (obj == SCM_BOOL_T) {
      ntabs++;
    } else if (WLDECOP(obj)) {
      nplugs++;
      WL_DECO(obj)->assure_free_deco(tag, i+1);
    } else {
      gwm_wrong_type_arg(tag, i+1, obj, "deco, pixmap, active pixmap, '(), #t or #f");
    }
  }
  plugs = new Decoration*[nplugs];
  for (i = 0; i < nplugs; i++)
    plugs[i] = 0; // no dangling pointers - command below may cause GC
  scm_gc_protect_object(self); // prevent GC during creation
  for (i=0, arg=sub; SCM_NNULLP(arg); arg=SCM_CDR(arg)) {
    obj = SCM_CAR(arg);
    if (WLPIXMAPP(obj) || WLACTIVEP(obj)) {
      plugs[i] = new Plug(obj);
      plugs[i]->parent = this;
      i++;
    } else if (WLDECOP(obj)) {
      plugs[i] = WL_DECO(obj);
      if (plugs[i]->check_free_menu())
        plugs[i] = ((IMenu*)plugs[i])->unrealize();
      plugs[i]->parent = this;
      i++;
    } else if (obj == SCM_EOL) {
      plugs[i++] = NULL;
    } 
  }
  box.borderwidth = (borderwidth < 0 ? 0 : borderwidth);
  box.shape_flags = 0;
  if (WLPAINTP(background)) {
    box.set_trans_back(WL_PAINT(background)->IsShapedT());
    box.set_hole_back(WL_PAINT(background)->IsShapedH());
  }
  if (WLPAINTP(bordercolor)) {
    box.set_trans_border(WL_PAINT(bordercolor)->IsShapedT());
    box.set_hole_border(WL_PAINT(bordercolor)->IsShapedH());
  }
  updatepos(0, 0);
  ReconfigureUp(-1, 0, -1, 0, 0);
  scm_gc_unprotect_object(self);
}

Bar::~Bar()
{
  delete [] plugs;
}

Plug::Plug(SCM sub)
  : Decoration()
{
  graphic = sub;
  box.borderwidth = 0;
  box.shape_flags = 0;
  if (WLPIXMAPP(graphic)) {
    WL_PIXMAP(graphic)->register_use(this, 0);
    box.set_trans_back(WL_PAINT(graphic)->IsShapedT());
    box.set_hole_back(WL_PAINT(graphic)->IsShapedH());
  } else if (WLACTIVEP(graphic))
    WL_ACTIVE(graphic)->register_use(this);
  updatepos(0, 0);
  self = deco2scm(this);
  ReconfigureTurn(0);
}

Plug::~Plug()
{
  if (WLPIXMAPP(graphic))
    WL_PIXMAP(graphic)->unregister_use(this, 0);
  else if (WLACTIVEP(graphic))
    WL_ACTIVE(graphic)->unregister_use(this);
}

IMenu::IMenu(Decoration* mn, ScreenContext* scr)
  : InnerDeco()
{
  menu = mn;
  screen = scr;

  if (menu->parent) {
    menu = 0;
    delete this;
    gwm_misc_error(0, "menu already has a parent: ~A", mn->scm());
  }
  menu->parent = this;
  menu->updatepos(0, 0);

  /* create window now */
  orig_borderwidth = -1;
  borderwidth = menu->borderwidth;
  box.borderwidth = 0;
  box.x = 0;
  box.y = 0;
  box.width = menu->Width() + 2*menu->Borderwidth();
  box.height = menu->Height() + 2*menu->Borderwidth();
  box.shape_flags = 0;
  box.set_hole_parts(menu->Shaped());
  if (menu->floating) {
    floating = 1;
    anchor = menu->anchor; // save the "old" anchor here
    adescr = new Anchor(*menu->adescr);
  }
  ReconfigureTurn(0);
  CreateWindow(scr->Root());
  XSaveContext(dpy, hook, deco_context, (char *)this);
  fsm = new Fsm(this, 0, MenuMask);
  fsm->install_nullstate();
  menu->semiopen(0, screen);
  decoproc = 0;
  iconproc = 0;
  self = menu->scm();
  SCM_SETCDR(menu->scm(), (SCM) this);
  menu->status |= InternMenuStatus;
  status = MenuStatus;
  valid = -1;
}

IMenu::~IMenu()
{
  XDeleteContext(dpy, hook, deco_context);
  XDestroyWindow(dpy, hook);
  if (menu)
    delete menu;
  delete fsm;
}

Decoration* IMenu::unrealize()
{
  Decoration* ret;
  SCM_SETCDR(menu->scm(), (SCM) menu);
  menu->parent = 0;
  menu->close(0);
  ReconfigureTurn(0);
  ret = menu;
  menu = 0;
  delete this;
  return ret;
}  

void IMenu::pop(int x, int y, Decoration* par, class WlCursor* cursor, int grab_kbd, int grab_cld, int grab_cnf, int grab_nofrz)
{
  XSetWindowAttributes wa;
  unsigned int  w, h, bw, dp;
  Window root;
  int rubber = GWM_rubber_feedback;
  if (rubber) {
    UnDrawRubber();
    GWM_rubber_feedback = 0;
    Fsm::TempFreezeOff();
  }
  if (!menu->open(MenuStatus, 0, screen)) {
    gwm_misc_error(0, "Failed to open menu ~A", scm());
  }
  menu->status |= InternMenuStatus;
  valid = 3;
  if (box.hole_parts())
    internal_shape();
  menu->execopen();
  if (!(menu->status & InternMenuStatus)) {
    gwm_misc_error(0, "Failed to open menu ~A", scm());
  }
  changepos(x, y);
  wa.override_redirect = 1;
  XChangeWindowAttributes(dpy, hook, CWOverrideRedirect, &wa);
  XMoveResizeWindow(dpy, hook, -1-2*box.borderwidth, -1-2*box.borderwidth, 1, 1);
  XMapRaised(dpy, hook);
  set_active_grab(par, cursor, 0, grab_kbd, grab_cld, grab_cnf, grab_nofrz, 1);
  if (box.nonzerosize())
    XMoveResizeWindow(dpy, hook, box.x, box.y, box.width, box.height);
  else if (box.borderwidth)
    XMoveResizeWindow(dpy, hook, box.x, box.y,
                      box.width + 2 * box.borderwidth, box.height + 2 * box.borderwidth);
  issue_map_event();
  XSync(dpy, 0);
  //process_masked_events(ExposureMask);
  if (rubber) {
    Fsm::TempFreezeOn();
    ReDrawRubber();
    GWM_rubber_feedback = 1;
  }
}

void IMenu::unpop()
{
  int rubber;
  rubber = GWM_rubber_feedback;
  if (rubber) {
    UnDrawRubber();
    GWM_rubber_feedback = 0;
    Fsm::TempFreezeOff();
  }
  CatchUnmapRelease(this);
  XUnmapWindow(dpy, hook);
// XSync needed here ?
  //process_masked_events(EnterWindowMask);
  remove_active_grab(1);
  issue_unmap_event();
  ProcessGwmEvents();
  menu->execclose();
  menu->close(1);
  menu->status = InternMenuStatus;
  valid = -1;
  XSync(dpy, 0);
  if (rubber) {
    //process_masked_events(ExposureMask);
    Fsm::TempFreezeOn();
    ReDrawRubber();
    GWM_rubber_feedback = 1;
  }
}


IClient::IClient(Window win, ClientWindow* cw, ScreenContext* scr)
  : InnerDeco()
{
  unsigned int depth, bw, width, height;
  int x, y;
  Window root;
  wind = win;
  window = cw;
  screen = scr;
  has_client = 1;
  if (!XGetGeometry(dpy, wind, &root,
                    &x, &y, &width, &height, &bw, &depth)) {
    self = deco2scm(this);   // It is dead already, do minimal stuff and return
    valid = -1;
    status = ClientStatus;
    wind = 0;
    return;
  }
  borderwidth = -1;
  orig_borderwidth = bw;
  inner_borderwidth = bw;
  SetOrigPos(x, y);
  box.borderwidth = 0;
  box.x = 0;
  box.y = 0;
  box.shape_flags = 0;
  box.width = width + 2*bw;
  box.height = height + 2*bw;
  if (GWM_ShapeExtension) {
    int xws, yws, xbs, ybs;
    unsigned int wws, hws, wbs, hbs;
    int boundingShaped, clipShaped;
    if (XShapeQueryExtents (dpy, wind,
                            &boundingShaped, &xws, &yws, &wws, &hws,
                            &clipShaped, &xbs, &ybs, &wbs, &hbs) &&
        boundingShaped)
      box.set_hole_parts(1);
  }
  self = deco2scm(this);
  status = ClientStatus;
  valid = -1;
  ReconfigureTurn(0);
}

UScreen::UScreen(ScreenContext* scr)
  : Decoration()
{
  number = scr->ScreenNum();
  parent = 0;
  window = 0;
  screen = scr;
  box.x = 0;
  box.y = 0;
  box.borderwidth = 0;
  box.shape_flags = 0;
  box.width = scr->width;
  box.height = scr->height;
  eflags = 0;
  self = deco2scm(this);
  scm_gc_protect_object(self);
  valid = -1;
}

void Decoration::semiopen(class ClientWindow* cw, class ScreenContext* scr)
{
  XSetWindowAttributes wa;
  if (valid == 0) {
    window = cw;
    screen = scr;
    CreateWindow(parent ? parent->Xwin() : screen->Root());
    XSaveContext(dpy, hook, deco_context, (char *)this);
    if (parent == NULL) {
      wa.override_redirect = 1;
      XChangeWindowAttributes(dpy, hook, CWOverrideRedirect, &wa);
    }
  }
}

void Bar::semiopen(class ClientWindow* cw, class ScreenContext* scr)
{
  int i;
  Decoration::semiopen(cw, scr);
  if (valid == 0) {
    fsm = new Fsm(this, (parent ? parent->fsm : screen->Deco()->fsm), WobMask);
    if (WLBEHAVIORP(behavior))
      fsm->install_state(WL_BEHAVIOR(behavior));
    else
      fsm->install_nullstate();
    valid = -1;
  }
  for (i = 0; i < nplugs; i++)
    if (plugs[i])
      plugs[i]->semiopen(cw, scr);
}

void Plug::semiopen(class ClientWindow* cw, class ScreenContext* scr)
{
  Decoration::semiopen(cw, scr);
  if (valid == 0) {
    fsm = new Fsm(this, (parent ? parent->fsm : screen->Deco()->fsm),
                  (WLACTIVEP(graphic) ? WobMask | ExposureMask :WobMask));
    fsm->install_nullstate();
    valid = -1;
  }
}

void Decoration::execopen()
{
  XEvent* evt;
  evt = new XEvent;
  evt->xany.type = GWMOpeningEvent;
  evt->xany.display = 0;
  evt->xany.window = (Window) 0;
  evt->xany.send_event = 1;                /* always propagate further */
  EventHandler(evt);
  delete evt;
}

void Decoration::execclose()
{
  XEvent* evt;
  evt = new XEvent;
  evt->xany.type = GWMClosingEvent;
  evt->xany.display = 0;
  evt->xany.window = (Window) 0;
  evt->xany.send_event = 1;                /* always propagate further */
  EventHandler(evt);
  delete evt;
}

int Decoration::open(int stat, class ClientWindow* cw, class ScreenContext* scr)
{
  XSetWindowAttributes wa;
  window = cw;
  if (valid == 0) {
    screen = scr;
    CreateWindow(parent ? parent->Xwin() : screen->Root());
    XSaveContext(dpy, hook, deco_context, (char *)this);
    if (parent == NULL) {
      wa.override_redirect = 1;
      XChangeWindowAttributes(dpy, hook, CWOverrideRedirect, &wa);
    }
  }
  if (self) {
    scm_gc_protect_object(self);
  }
  status = stat;
  return 1;
}

int Bar::open(int stat, class ClientWindow* cw, class ScreenContext* scr)
{
  int i, j1 = -1, j2 = -1;
  if (status != NoStatus && status != InternMenuStatus) {
    gwm_warning("Attempt to re-open an opened decoration: ~A", scm());
    return 0;
  }
  Decoration::open(stat, cw, scr);
  if (valid == 0) {
    fsm = new Fsm(this, (parent ? parent->fsm : screen->Deco()->fsm), WobMask);
    if (WLBEHAVIORP(behavior))
      fsm->install_state(WL_BEHAVIOR(behavior));
    else
      fsm->install_nullstate();
  }
  valid = 1;
  if (WLPAINTP(background))
    WL_PAINT(background)->paint_background(hook, &box);
  if (WLPAINTP(bordercolor))
    WL_PAINT(bordercolor)->paint_border(hook, &box);
  if (WLCURSORP(cursor))
    WL_CURSOR(cursor)->use_cursor(hook);
  for (i = 0; i < nplugs; i++)
    if (plugs[i]) {
      if (!plugs[i]->open(stat, cw, scr)) {
        int j;
        for (j = 0; j < i; j++)
          if (plugs[j])
            plugs[j]->close(0);
        Decoration::close(0);
        return 0;
      }
      if (!plugs[i]->floating) 
        j2 = i;
      else if (j1 != -1)
        j1 = i;
    }
  if (j1 != -1)
    for (i = j1; i < j2; i++)
      if (plugs[i] && plugs[i]->floating)
        maintain_stacking(i);
  if (box.shaped())
    paint_shape();
  if (!hidden && parent)
    XMapWindow(dpy, hook);
  return 1;
}

int Plug::open(int stat, class ClientWindow* cw, class ScreenContext* scr)
{
  if (status != NoStatus && status != InternMenuStatus) {
    gwm_warning("Attempt to re-open an opened decoration: ~A", scm());
    return 0;
  }
  Decoration::open(stat, cw, scr);
  if (WLPIXMAPP(graphic))
    WL_PAINT(graphic)->paint_background(hook, &box);
  if (box.shaped_back())
    paint_shape();
  if (valid == 0) {
    fsm = new Fsm(this, (parent ? parent->fsm : screen->Deco()->fsm),
                  (WLACTIVEP(graphic) ? WobMask | ExposureMask :WobMask));
    fsm->install_nullstate();
  }
  valid = 1;
  if (!hidden && parent)
    XMapWindow(dpy, hook);
  return 1;
}

int IMenu::open(int stat, class ClientWindow* cw, class ScreenContext* scr)
{
  XSetWindowAttributes wa;
  if (status != MenuStatus) {
    gwm_warning("Attempt to re-open an opened decoration: ~A", scm());
    return 0;
  }
  if (!window || cw != window) {
    gwm_warning("Attempt to open the wrong menu: ~A", scm());
    return 0;
  }
  status |= stat;
  if (!menu->open(status, cw, scr)) {
    gwm_warning("Failed to open menu ~A", scm());
    return 0;
  }
  menu->status |= InternMenuStatus;
  valid = 1;
  if (box.hole_parts())
    internal_shape();
  if (parent) {
    if (TrapXErrors(XReparentWindow(dpy, hook, parent->Xwin(), box.x, box.y)))
      return 0;
    fsm->change_parent(parent->fsm);
  } else {
    wa.override_redirect = (!decoproc ? 0 : 1);
    XChangeWindowAttributes(dpy, hook, CWOverrideRedirect, &wa);
    MoveWindow();
    fsm->change_parent(screen->Deco()->fsm);
  }
  scm_gc_protect_object(self);
  if (!hidden && parent)
    XMapWindow(dpy, hook);
  return 1;
}

int IClient::open(int stat, class ClientWindow* cw, class ScreenContext* scr)
{
  unsigned int client_height, client_width, client_bw;
  int client_x, client_y;
  Window dummy1;
  unsigned int dummy2;
  XSetWindowAttributes wa;

  if (status != ClientStatus) {
    gwm_warning("Attempt to re-open an opened client window: ~A", scm());
    return 0;
  }
  if (!window || cw != window) {
    gwm_warning("Attempt to open the wrong client: ~A", scm());
    return 0;
  }
  CreateWindow(parent ? parent->Xwin() : screen->Root());
  XSaveContext(dpy, hook, deco_context, (char *)this);
  if (parent == NULL) {
    wa.override_redirect = 1;
    XChangeWindowAttributes(dpy, hook, CWOverrideRedirect, &wa);
  }
  scm_gc_protect_object(self);
  status = stat | ClientStatus;
  valid = 1;
  WL_PAINT(screen->pixel.White)->paint_background(hook, &box);
  if (WLCURSORP(cursor))
    WL_CURSOR(cursor)->use_cursor(hook);
  if (GWM_rubber_feedback)
    UnDrawRubber();
  XUnmapWindow(dpy, wind);
  if (GWM_rubber_feedback)
    ReDrawRubber();
  XSync(dpy, False);
  XSelectInput(dpy, wind, ClientClientMask);
  if (GWM_ShapeExtension)
    XShapeSelectInput(dpy, wind, True);
  XChangeSaveSet(dpy, wind, SetModeInsert);
  fsm = new ClientFsm(this, (parent ? parent->fsm : screen->Deco()->fsm), ClientMask);
  if (WLBEHAVIORP(behavior))
    fsm->install_state(WL_BEHAVIOR(behavior));
  else
    fsm->install_nullstate();
  if (TrapXErrors(XReparentWindow(dpy, wind, hook, 0, 0))) {
    wind = 0;        // Already dead
    close(0);
    return 0;
  }
  XSaveContext(dpy, wind, client_context, (caddr_t)this);
  /* check if window hasn't been resized since deco */
  if (!XGetGeometry(dpy, wind,
                    &dummy1, &client_x, &client_y, &client_width, 
                    &client_height, &client_bw, &dummy2)) {
    wind = 0;        // Already dead
    close(0);
    return 0;
  }
  if (box.hole_parts())
    internal_shape();
  if (client_height + 2 * client_bw != box.height ||
      client_width + 2 * client_bw != box.width) {
    intern_set_borderwidth(client_bw);
    box.height = client_height + 2 * client_bw;
    box.width = client_width + 2 * client_bw;
    ReconfigureUp(-1, 0, 0, 0, 0);
  } else
    SendSyntheticMoveEvent(inner_x+window->Deco()->box.x,
                           inner_y+window->Deco()->box.y);
  XMapWindow(dpy, wind);
  if (!hidden && parent)
    XMapWindow(dpy, hook);
  return 1;
}

int UScreen::open(int stat, class ClientWindow* cw, class ScreenContext* scr)
{
  if (status != NoStatus) {
    gwm_warning("Attempt to re-open an opened screen: ~A", scm());
    return 0;
  }
  if (!scr || scr->Deco() != this) {
    gwm_warning("Attempt to open the wrong screen: ~A", scm());
    return 0;
  }
  hook = screen->Root();
  XSaveContext(dpy, hook, deco_context, (char *)this);
  if (WLPAINTP(background))
    WL_PAINT(background)->paint_background(hook, &box);
  if (WLCURSORP(cursor))
    WL_CURSOR(cursor)->use_cursor(hook);
  fsm = new Fsm(this, 0, (GWM_WidgetMode ? RootMask2 : RootMask));
  if (WLBEHAVIORP(behavior))
    fsm->install_state(WL_BEHAVIOR(behavior));
  else
    fsm->install_nullstate();
  status = stat;
  valid = 1;
  return 1;
}

void Decoration::close(int flag)
{
  if (valid != 0) {
    if (flag != 1) {
      XDeleteContext(dpy, hook, deco_context);
      XDestroyWindow(dpy, hook);
      delete fsm;
      fsm = 0;
      screen = 0;
    }
    window = 0;
    if (self && valid > 0) {
      scm_gc_unprotect_object(self);
    }
    status = NoStatus;
    valid = (flag == 1 ? -1 : 0);
  }
}

void Bar::close(int flag)
{
  int i;
  for (i = 0; i < nplugs; i++)
    if (plugs[i])
      plugs[i]->close(flag);
  Decoration::close(flag);
}

void IMenu::close(int flag)
{
  int xoff, yoff;
  if (valid <= 0)
    return;
  if (flag == 2) {
    valid = 4;
    return;
  }
  valid = -1;
  XUnmapWindow(dpy, hook);
  if (parent) {
    if (this == window->Inner()) {  /* Only windows have gravity */
      window->CalcGravityOffset(xoff, yoff); 
      updatepos(window->Deco()->Xpos() + xoff, window->Deco()->Ypos() + yoff);
      XReparentWindow(dpy, hook, screen->Root(), 
                      menu->Xpos(), menu->Ypos());
      XSetWMNormalHints(dpy, hook, window->NormalHints());
    } else {
      updatepos(Top()->Xpos(), Top()->Ypos());
      XReparentWindow(dpy, hook, screen->Root(), 
                      menu->Xpos(), menu->Ypos());
    }
    fsm->change_parent(0);
    parent->remove_part(this);
  }
  window = 0;
  if (menu->borderwidth != -1 && orig_borderwidth != -1 && orig_borderwidth != menu->borderwidth) {
    menu->borderwidth = borderwidth;
    menu->intern_set_borderwidth(orig_borderwidth);
    ReconfigureTurn(0);
  }
  menu->close(1);
  menu->status = InternMenuStatus;
  status = MenuStatus;
  scm_gc_unprotect_object(self);
}

void IClient::close(int flag)
{
  int xoff, yoff;
  if (valid <= 0)
    return;
  if (valid != 4 && wind) {
    XSelectInput(dpy, wind, 0);
    if (GWM_ShapeExtension)
      XShapeSelectInput(dpy, wind, False);
    XSync(dpy, False);
    if (this == window->Client()) {  /* Only windows have gravity */
      window->CalcGravityOffset(xoff, yoff); 
      XReparentWindow(dpy, wind, screen->Root(), 
                      window->Deco()->box.x + xoff, window->Deco()->box.y + yoff);
      XSetWMNormalHints(dpy, wind, window->NormalHints());
      SendSyntheticMoveEvent(window->Deco()->box.x + xoff, window->Deco()->box.y + yoff);
    } else { /* Icon presumably */
      XUnmapWindow(dpy, wind);
      XReparentWindow(dpy, wind, screen->Root(), 
                      Top()->box.x, Top()->box.y);
      SendSyntheticMoveEvent(Top()->box.x, Top()->box.y);
    }
    XRemoveFromSaveSet(dpy, wind);
    if (orig_borderwidth != inner_borderwidth)
      XSetWindowBorderWidth(dpy, wind, orig_borderwidth);
    XDeleteContext(dpy, wind, client_context);
  }
  if (flag==2) {
    valid = 4;
  } else {
    Decoration::close(0);
    valid = -1;
    if (parent)
      parent->remove_part(this);
    status = ClientStatus;
  }
}

void UScreen::close(int flag)
{
  XDeleteContext(dpy, hook, deco_context);
  delete fsm;
  fsm = 0;
  valid = -1;
  status = NoStatus;
  scm_gc_unprotect_object(self);
}

// eflags: LONGH 1, LONGV 2, EXTH 4, EXTHUP 8, EXTV 16, EXTVUP 32, TABS 64
void Bar::setnaturalsize(short pdir)
{
  int i, nr = 0, nrh = 0, nrv = 0;
  int currw, currh;
  eflags = 0;
  if (direction == HORIZONTAL || direction == CENTER) {
    pdir = 1;
  } else if (floating || direction == VERTICAL) {
    pdir = 0;
  }
  for (i = 0; i < nplugs; i++) {
    if (plugs[i] == NULL) {
      nr++, nrh++, nrv++;
    } else {
      plugs[i]->setnaturalsize(!pdir);
      if (!plugs[i]->floating) {
        nr++;
        if (plugs[i]->eflags & 4)
          nrh++;
        if (plugs[i]->eflags & 16)
          nrv++;
      }
    }
  }
  if (direction != CENTER)
    eflags |= (pdir ? 1 : 2);
  if (width == -1 && (max_width == -1 || max_width != min_width))
    if (eflags & 1 ? nrh : nr && nrh==nr)
      eflags |= 4;
  if (height == -1 && (max_height == -1 || max_height != min_height))
    if (eflags & 2 ? nrv : nr && nrv==nr)
      eflags |= 16;
  if (eflags & 1) {
    currw = 0;
    currh = (min_height < 0 ? 0 : min_height);
    for (i = 0; i < nplugs; i++) 
      if (plugs[i] && !plugs[i]->floating) {
        currw += plugs[i]->box.width + 2 * plugs[i]->box.borderwidth + separator;
        if (eflags & 16 || !(plugs[i]->eflags & 16))
          currh = Max(plugs[i]->box.height + 2 * plugs[i]->box.borderwidth, currh);
      }
    if (currw)
      currw += 2 * margin - separator;
    else
      currw += 2 * margin;
  } else if (eflags & 2) {
    currh = 0;
    currw = (min_width < 0 ? 0 : min_width);
    for (i = 0; i < nplugs; i++) 
      if (plugs[i] && !plugs[i]->floating) {
        currh += plugs[i]->box.height + 2 * plugs[i]->box.borderwidth + separator;
        if (eflags & 4 || !(plugs[i]->eflags & 4))
          currw = Max(plugs[i]->box.width + 2 * plugs[i]->box.borderwidth, currw);
      }
    if (currh)
      currh += 2 * margin - separator;
    else
      currh += 2 * margin;
  } else {
    currw = 0;
    currh = 0;
    for (i = 0; i < nplugs; i++) 
      if (plugs[i] && !plugs[i]->floating) {
        if (eflags & 16 || !(plugs[i]->eflags & 16))
          currh = Max(plugs[i]->box.height + 2 * plugs[i]->box.borderwidth, currh);
        if (eflags & 4 || !(plugs[i]->eflags & 4))
          currw = Max(plugs[i]->box.width + 2 * plugs[i]->box.borderwidth, currw);
      }
    currw += 2 * margin;
    currh += 2 * margin;
  }
  if (width != -1)
    currw = width;
  else if (max_width >= 0)
    currw = Min(currw, max_width);
  else if (min_width >= 0)
    currw = Max(currw, min_width);
  if (height != -1)
    currh = height;
  else if (max_height >= 0)
    currh = Min(currh, max_height);
  else if (min_height >= 0)
    currh = Max(currh, min_height);
  box.width = currw;
  box.height = currh;
}

void Plug::setnaturalsize(short pdir)
{
  Box b;
  eflags = 0;
  if (WLPIXMAPP(graphic))
    WL_PIXMAP(graphic)->dimensions(&b);
  else if (WLACTIVEP(graphic)) {
    if (WL_ACTIVE(graphic)->width == -1)
      eflags |= 4;
    if (WL_ACTIVE(graphic)->height == -1)
      eflags |= 16;
    WL_ACTIVE(graphic)->intrinsic_dimensions(&b);
  } else
    b.width = b.height = 0;
  box.width = b.width;
  box.height = b.height;
}

void Decoration::changepos(int x, int y)
{
  int ix, iy, d1, d2;
  box.x = x;
  box.y = y;
  if (valid > 0) {
    MoveWindow();
    if ((status & WindowStatus) && window->Client()) {
      window->Client()->GetInnerDims(ix, iy, d1, d2);
      window->Client()->SendSyntheticMoveEvent(ix + box.x, iy + box.y);
    }
  }
}

void Decoration::updatepos(int x, int y)
{
  box.x = x;
  box.y = y;
}

int Decoration::calclength(int dir, int delta, int& ext)
{
  int tmp;
  int size = 2 * box.borderwidth + (dir ? box.width : box.height);
  ext = 0;
  if (eflags & (dir ? 4 : 16)) {
    size += delta; 
    if (delta >= 0) {
      tmp = (dir ? max_width : max_height);
      if (tmp != -1 && size >= tmp)
        size = tmp;
      else
        ext = 1;
    } else if (delta < 0) {
      tmp = Max((dir ? min_width : min_height), 0);
      if (size <= tmp)
        size = tmp;
      else
        ext = 1;
    }
  }
  return size;
}

void Bar::updatesize(int xs, int ys)
{
  int i, currsize, total_space, x, y, w, h, ext, nsep;
  int n = 0, n_spaces = 0, old_n_spaces = -1, delta = 0, shift = 0;
  if (xs != -1) {
    box.width = xs - 2 * box.borderwidth;
    if (box.width < 1) box.width = 0;
  }
  if (ys != -1) { 
    box.height = ys - 2 * box.borderwidth;
    if (box.height < 1) box.height = 0;
  }
  if (eflags & 3) {
    // calc total_space, n_spaces, delta, shift
    while (n_spaces != old_n_spaces) {
      old_n_spaces = n_spaces;
      n_spaces = 0;
      nsep = 0;
      total_space = (eflags & 1 ? box.width : box.height) - 2*margin;
      for (i = 0; i < nplugs; i++)
        if (!plugs[i]) {
          if (delta >= 0)
            n_spaces++;
        } else if (!plugs[i]->floating) {
          total_space -= plugs[i]->calclength((eflags & 1 ? 1 : 0), delta, ext);
          nsep++;
          if (ext) {
            n_spaces++;
            total_space += delta;
          }
        }
      if (nsep)
        total_space -= separator*(nsep-1); 
      if (n_spaces && (total_space != 0)) {
        delta = total_space / n_spaces;
        shift = total_space % n_spaces;
        n = shift;
      }
    }
  }
  currsize = margin;
  for (i = 0; i < nplugs; i++)
    if (plugs[i]) {
      if (plugs[i]->floating) {
        interpret_anchor(plugs[i]->adescr, &box, &plugs[i]->box, x, y, w, h);
        plugs[i]->box.x = x;
        plugs[i]->box.y = y;
        plugs[i]->updatesize(w, h);
      } else if (eflags & 1) {
        h = plugs[i]->calclength(0, box.height - plugs[i]->box.height - 2 * plugs[i]->box.borderwidth, ext);
        w = plugs[i]->calclength(1, delta, ext);
        if (ext) {
          if (n + shift >= n_spaces) w++;
          n = (n + shift) % n_spaces;
        }
        plugs[i]->box.x = currsize;
        plugs[i]->box.y = (int)(box.height - h) / 2;
        plugs[i]->updatesize(w, h);
        currsize += w + separator;
      } else if (eflags & 2) {
        w = plugs[i]->calclength(1, box.width - plugs[i]->box.width - 2 * plugs[i]->box.borderwidth, ext);
        h = plugs[i]->calclength(0, delta, ext);
        if (ext) {
          if (n + shift >= n_spaces) h++;
          n = (n + shift) % n_spaces;
        }
        plugs[i]->box.y = currsize;
        plugs[i]->box.x = (int)(box.width - w) / 2;
        plugs[i]->updatesize(w, h);
        currsize += h + separator;
      } else {
        w = plugs[i]->calclength(1, box.width - plugs[i]->box.width - 2 * plugs[i]->box.borderwidth - 2 * margin, ext);
        h = plugs[i]->calclength(0, box.height - plugs[i]->box.height - 2 * plugs[i]->box.borderwidth - 2 * margin, ext);
        plugs[i]->box.x = (int)(box.width - w) / 2;
        plugs[i]->box.y = (int)(box.height - h) / 2;
        plugs[i]->updatesize(w, h);
      }
    } else if (eflags & 3) {
      if (delta > 0) {
        currsize += delta + (n + shift >= n_spaces ? 1 : 0);
        n = (n + shift) % n_spaces;
      }
    }
}

void Plug::updatesize(int xs, int ys)
{
  int ok = 0;
  if (WLACTIVEP(graphic)) {
    if (WL_ACTIVE(graphic)->width == -1) {
      box.width = (xs != -1 ? xs : 0);
      ok = 1;
    }
    if (WL_ACTIVE(graphic)->height == -1) { 
      box.height = (ys != -1 ? ys : 0);
      ok = 1;
    }
    if (ok)
      WL_ACTIVE(graphic)->mark_dirty_one(this);
  }
}

void IMenu::updatesize(int xs, int ys)
{
  // Ignore new size but calculate new inner position
  int dx, dy;
  Decoration* dec;
  for (dec=this, dx=0, dy=0; dec->parent; dec=dec->parent) {
    dx += dec->box.x + dec->box.borderwidth;
    dy += dec->box.y + dec->box.borderwidth;
  }
  inner_x = dx + menu->Borderwidth() + dec->box.borderwidth;
  inner_y = dy + menu->Borderwidth() + dec->box.borderwidth;
}

void IClient::updatesize(int xs, int ys)
{
  // Ignore new size but calculate new inner position
  int dx, dy;
  Decoration* dec;
  for (dec=this, dx=0, dy=0; dec->parent; dec=dec->parent) {
    dx += dec->box.x + dec->box.borderwidth;
    dy += dec->box.y + dec->box.borderwidth;
  }
  dx += inner_borderwidth + dec->box.borderwidth;
  dy += inner_borderwidth + dec->box.borderwidth;
  if (dx != inner_x || dy != inner_y) {
    inner_x = dx;
    inner_y = dy;
    inner_pos_dirty = 1;
  }
}

int Decoration::compile_acord(SCM ac, float& out, float& in, int& off)
{
  int tmp;
  if (scm_is_integer(ac)) {
    tmp = gh_scm2int(ac);
    if (tmp < 0) {
      out = 1.0;
      in = 1.0;
      off = tmp + 1;
    } else {
      out = 0.0;
      in = 0.0;
      off = tmp;
    }
    return 1;
  } else if (scm_ilength(ac) == 3 &&
             scm_real_p(SCM_CAR(ac)) &&
             scm_integer_p(SCM_CAR(SCM_CDR(ac))) &&
             scm_real_p(SCM_CAR(SCM_CDR(SCM_CDR(ac))))) {
    out = gh_scm2double(SCM_CAR(ac));
    in = gh_scm2double(SCM_CAR(SCM_CDR(SCM_CDR(ac))));
    off = gh_scm2int(SCM_CAR(SCM_CDR(ac)));
    return 1;
  } else if (ac == SCM_EOL) {
    out = 0.5;
    in = 0.5;
    off = 0;
    return 1;
  } else if (ac == SCM_BOOL_F) {
    return 0;
  } else 
    return -1;
}

void Decoration::compile_anchor(SCM anchor, Anchor* descr, const char* func)
{
  int res, off;
  float out, in;
  int n;
  if (anchor == SCM_EOL) {
    descr->numx = 0;
    descr->numy = 0;
    return;
  }
  n  = scm_ilength(anchor);
  if (n < 2 || n > 4) 
    gwm_wrong_type_arg(func, 0, anchor, "anchor list");
  res = compile_acord(SCM_CAR(anchor), out, in, off);
  if (res == 1) {
    descr->outx1 = out;
    descr->inx1 = in;
    descr->offx1 = off;
    descr->numx = 1;
  } else
    gwm_wrong_type_arg(func, 0, SCM_CAR(anchor), "anchor element");
  res = compile_acord(SCM_CAR(SCM_CDR(anchor)), out, in, off);
  if (res == 1) {
    descr->outy1 = out;
    descr->iny1 = in;
    descr->offy1 = off;
    descr->numy = 1;
  } else
    gwm_wrong_type_arg(func, 0, SCM_CAR(SCM_CDR(anchor)), "anchor element");
  if (n >= 3 && (res = compile_acord(SCM_CAR(SCM_CDR(SCM_CDR(anchor))), out, in, off))) {
    if (res == -1) 
      gwm_wrong_type_arg(func, 0, SCM_CAR(SCM_CDR(SCM_CDR(anchor))), "anchor element");
    if (res == 1 && in != descr->inx1) {
      descr->outx2 = out;
      descr->inx2 = in;
      descr->offx2 = off;
      descr->numx = 2;
    }
  }
  if (n == 4 && (res = compile_acord(SCM_CAR(SCM_CDR(SCM_CDR(SCM_CDR(anchor)))), out, in, off))) {
    if (res == -1) 
      gwm_wrong_type_arg(func, 0, SCM_CAR(SCM_CDR(SCM_CDR(SCM_CDR(anchor)))), "anchor element");
    if (res == 1 && in != descr->iny1) {
      descr->outy2 = out;
      descr->iny2 = in;
      descr->offy2 = off;
      descr->numy = 2;
    }
  }
}
  
void Decoration::interpret_anchor(Anchor* descr, Box* b1, Box* b2, int& x, int& y, int& w, int& h)
{
  float ww;
  if (!descr || !descr->numx || !descr->numy) {
    x=b2->x, y=b2->y, w=h=-1;
    return;
  }
  if (descr->numx == 2) {
    ww = (b1->width * (descr->outx1 - descr->outx2) +
          (descr->offx1 - descr->offx2)) /
         (descr->inx1 - descr->inx2);
    if (ww <= 0.0) {
      x = Round(b1->width * descr->outx1 + descr->offx1);
      w = 0;
    } else {
      x = Round(b1->width * descr->outx1 + descr->offx1 -
                descr->inx1 * ww);
      w = Round(ww);
    }
  } else {
    x = Round(b1->width * descr->outx1 + descr->offx1 -
              (b2->width + 2*b2->borderwidth) * descr->inx1);
    w = -1;
  }
  if (descr->numy == 2) {
    ww = (b1->height * (descr->outy1 - descr->outy2) +
          (descr->offy1 - descr->offy2)) /
         (descr->iny1 - descr->iny2);
    if (ww <= 0.0) {
      y = Round(b1->height * descr->outy1 + descr->offy1);
      h = 0;
    } else {
      y = Round(b1->height * descr->outy1 + descr->offy1 -
                descr->iny1 * ww);
      h = Round(ww);
    }
  } else {
    y = Round(b1->height * descr->outy1 + descr->offy1 -
              (b2->height + 2*b2->borderwidth) * descr->iny1);
    h = -1;
  }
}

void Bar::disable_parts()
{
  int i;
  for (i = 0; i < nplugs; i++)
    if (plugs[i]) {
      if (!plugs[i]->hidden) {
        CatchUnmapRelease(plugs[i]);
        XUnmapWindow(dpy, plugs[i]->hook);
      }
      plugs[i]->hidden |= 2;
    }
}

void Bar::enable_parts()
{
  int i;
  for (i = 0; i < nplugs; i++)
    if (plugs[i]) {
      plugs[i]->hidden &= ~2;
      if (!plugs[i]->hidden)
        XMapWindow(dpy, plugs[i]->hook);
    }
}

void Bar::changed_stacking(Decoration* child)
{
  int i;
  for (i = 0; i < nplugs; i++)
    if (plugs[i] == child) {
      maintain_stacking(i);
      break;
    }
}

void Decoration::exec_recursive_event(XEvent* ev, int noreconf)
{
  fsm->action(ev);
}

void Bar::exec_recursive_event(XEvent* ev, int noreconf)
{
  int i;
  int old_df = delay_reconfigure;
  delay_reconfigure = 1;
  Decoration::exec_recursive_event(ev, 1);
  for (i = 0; i < nplugs; i++)
    if (plugs[i])
      plugs[i]->EventHandler(ev);
  delay_reconfigure = old_df;
  if (!old_df && !noreconf)
    ForceReconfigure();
}

void IMenu::exec_recursive_event(XEvent* ev, int noreconf)
{
  menu->exec_recursive_event(ev, noreconf);
}

void Decoration::issue_map_event()
{
  if (valid > 0 && (fsm->GwmMask() & GWMMapMask))
    send_gwm_event(this, GWMMapEvent);
}

void Decoration::issue_unmap_event()
{
  if (valid > 0 && (fsm->GwmMask() & GWMUnmapMask))
    send_gwm_event(this, GWMUnmapEvent);
}

void Bar::issue_map_event()
{
  int i;
  Decoration::issue_map_event();
  for (i=0; i<nplugs; i++)
    if (plugs[i] && !plugs[i]->hidden)
      plugs[i]->issue_map_event();
}

void Bar::issue_unmap_event()
{
  int i;
  Decoration::issue_unmap_event();
  for (i=0; i<nplugs; i++)
    if (plugs[i] && !plugs[i]->hidden)
      plugs[i]->issue_unmap_event();
}

void IMenu::issue_map_event()
{
  Decoration::issue_map_event();
  if (!menu->hidden)
    menu->issue_map_event();
}

void IMenu::issue_unmap_event()
{
  Decoration::issue_unmap_event();
  if (!menu->hidden)
    menu->issue_unmap_event();
}

void Decoration::issue_resize_event()
{
  if (valid > 0 && (fsm->GwmMask() & GWMResizeMask))
    send_gwm_event(this, GWMResizeEvent);
}

void Decoration::issue_move_event()
{
  if (valid > 0 && (fsm->GwmMask() & GWMMoveMask))
    send_gwm_event(this, GWMMoveEvent);
}

void Decoration::issue_stack_event()
{
  if (valid > 0 && (fsm->GwmMask() & GWMStackMask))
    send_gwm_event(this, GWMStackEvent);
}

int Decoration::check_issue_stack_event()
{
  return (valid > 0 && (fsm->GwmMask() & GWMStackMask));
}

void Decoration::issue_enter_recursive(Decoration* cp, XCrossingEvent* ev)
{
  if (parent && cp != parent)
    parent->issue_enter_recursive(cp, ev);
  issue_enter_event(ev);
}

void Decoration::issue_enter_event(XCrossingEvent* ev)
{
  if (valid > 0 && fsm->GwmMask() & GWMEnterMask)
    send_gwm_crossing_event(this, ev, GWMEnterEvent);
}

void Decoration::issue_leave_event(XCrossingEvent* ev)
{
  if (valid > 0 && fsm->GwmMask() & GWMLeaveMask)
    send_gwm_crossing_event(this, ev, GWMLeaveEvent);
}

void Decoration::CreateWindow(Window par)
{
  oldbox = box;
  if (box.nonzerosize())
    hook = XCreateSimpleWindow(dpy, par, box.x, box.y, box.width, box.height,
                               box.borderwidth, 0, 0);
  else if (box.borderwidth)
    hook = XCreateSimpleWindow(dpy, par, box.x, box.y,
                               box.width + 2 * box.borderwidth,
                               box.height + 2 * box.borderwidth,
                               0, 0, 0);
  else
    hook = XCreateSimpleWindow(dpy, par, -1, -1, 1, 1, 0, 0, 0);
}

void Decoration::MoveWindow()
{
  int rubber = GWM_rubber_feedback && Visible();
  if (rubber) {
    UnDrawRubber();
    GWM_rubber_feedback = 0;
  }
  if (box.nonzerosize())
    XMoveWindow(dpy, hook, box.x, box.y);
  else if (box.borderwidth)
    XMoveWindow(dpy, hook, box.x, box.y);
  if (box.x != oldbox.x || box.y != oldbox.y) {
    issue_move_event();
    oldbox.x = box.x;
    oldbox.y = box.y;
  }
  if (rubber) {
    ReDrawRubber();
    GWM_rubber_feedback = 1;
  }
}

void Decoration::ResizeWindow()
{
  int rubber = GWM_rubber_feedback && Visible();
  if (rubber) {
    UnDrawRubber();
    GWM_rubber_feedback = 0;
  }
  if (oldbox.nonzerosize())
    if (box.nonzerosize()) {
      if (box.width != oldbox.width || box.height != oldbox.height)
        XResizeWindow(dpy, hook, box.width, box.height);
    } else if (box.borderwidth) {
      XResizeWindow(dpy, hook, 
                    box.width + 2 * box.borderwidth, box.height + 2 * box.borderwidth);
      XSetWindowBorderWidth(dpy, hook, 0);
      disable_parts();
      if (WLPAINTP(bordercolor))
        WL_PAINT(bordercolor)->paint_border(hook, &box);
    } else {
      XMoveResizeWindow(dpy, hook, -1, -1, 1, 1);
    }
  else if (oldbox.borderwidth) {
    if (box.nonzerosize()) {
      XResizeWindow(dpy, hook, box.width, box.height);
      XSetWindowBorderWidth(dpy, hook, box.borderwidth);
      enable_parts();
      if (WLPAINTP(background))
        WL_PAINT(background)->paint_background(hook, &box);
    }
  } else
    if (box.nonzerosize()) {
      XMoveResizeWindow(dpy, hook, box.x, box.y, box.width, box.height);
    }
  if (box.width != oldbox.width || box.height != oldbox.height) {
    issue_resize_event();
    oldbox.width = box.width;
    oldbox.height = box.height;
  }
  if (rubber) {
    ReDrawRubber();
    GWM_rubber_feedback = 1;
  }
}

void Decoration::MoveResizeWindow()
{
  int rubber = GWM_rubber_feedback && Visible();
  if (rubber) {
    UnDrawRubber();
    GWM_rubber_feedback = 0;
  }
  if (oldbox.nonzerosize())
    if (box.nonzerosize()) {
      if (box.width != oldbox.width || box.height != oldbox.height ||
          box.x != oldbox.x || box.y != oldbox.y)
        XMoveResizeWindow(dpy, hook, box.x, box.y, box.width, box.height);
    } else if (box.borderwidth) {
      XMoveResizeWindow(dpy, hook, box.x, box.y,
                        box.width + 2 * box.borderwidth, box.height + 2 * box.borderwidth);
      XSetWindowBorderWidth(dpy, hook, 0);
      disable_parts();
      if (WLPAINTP(bordercolor))
        WL_PAINT(bordercolor)->paint_border(hook, &box);
    } else {
      XMoveResizeWindow(dpy, hook, -1, -1, 1, 1);
    }
  else if (oldbox.borderwidth)
    if (box.nonzerosize()) {
      XMoveResizeWindow(dpy, hook, box.x, box.y, box.width, box.height);
      XSetWindowBorderWidth(dpy, hook, box.borderwidth);
      enable_parts();
      if (WLPAINTP(background))
        WL_PAINT(background)->paint_background(hook, &box);
    } else 
      XMoveWindow(dpy, hook, box.x, box.y);
  else
    if (box.nonzerosize()) {
      XMoveResizeWindow(dpy, hook, box.x, box.y, box.width, box.height);
    }
  if (box.width != oldbox.width || box.height != oldbox.height) {
    issue_resize_event();
    oldbox.width = box.width;
    oldbox.height = box.height;
    oldbox.x = box.x;
    oldbox.y = box.y;
  } else if (box.x != oldbox.x || box.y != oldbox.y) {
    issue_move_event();
    oldbox.x = box.x;
    oldbox.y = box.y;
  }
  if (rubber) {
    ReDrawRubber();
    GWM_rubber_feedback = 1;
  }
}

void Decoration::SetWindowBorderwidth()
{
  int rubber = GWM_rubber_feedback && Visible();
  if (rubber) {
    UnDrawRubber();
    GWM_rubber_feedback = 0;
  }
  if (box.nonzerosize()) {
    if (box.borderwidth != oldbox.borderwidth)
      XSetWindowBorderWidth(dpy, hook, box.borderwidth);
  } else {
    if (oldbox.borderwidth && box.borderwidth)
      XResizeWindow(dpy, hook,
                    box.width + 2 * box.borderwidth, box.height + 2 * box.borderwidth);
    else if (oldbox.borderwidth) {
      XMoveResizeWindow(dpy, hook, -1, -1, 1, 1);
      enable_parts();
      if (WLPAINTP(background))
        WL_PAINT(background)->paint_background(hook, &box);
    } else if (box.borderwidth) {
      XMoveResizeWindow(dpy, hook, box.x, box.y,
                        box.width + 2 * box.borderwidth, box.height + 2 * box.borderwidth);
      disable_parts();
      if (WLPAINTP(bordercolor))
        WL_PAINT(bordercolor)->paint_border(hook, &box);
    }
  }
  oldbox.borderwidth = box.borderwidth;
  if (rubber) {
    ReDrawRubber();
    GWM_rubber_feedback = 1;
  }
}

void Decoration::EventHandler(XEvent* evt)
{
  switch (evt->type) {
  case Expose:
    if (evt->xexpose.count == 0)
      XClearWindow(dpy, hook);
    break;
  case GWMExposeEvent:
    if (WLPAINTP(background))
      WL_PAINT(background)->paint_background(hook, &box);
    if (box.shaped_back() || box.hole_parts())
      ReconfigureUp(0, 0, -1, 0, 0);
    break;
  case GWMExposeBordEvent:
    if (WLPAINTP(bordercolor))
      WL_PAINT(bordercolor)->paint_border(hook, &box);
    if (box.shaped_border())
      ReconfigureUp(0, 0, -1, 0, 0);
    break;
  case EnterNotify:
    Fsm::register_enter(&evt->xcrossing, this);
    break;
  default:
    fsm->action(evt);
  }
}

void Bar::EventHandler(XEvent* evt)
{
  int i, old_df;
  if (evt->type == GWMUserEvent || evt->type == GWMOpeningEvent || evt->type == GWMClosingEvent) {
    if (nplugs && evt->xany.send_event)
      exec_recursive_event(evt, 0);
    else
      fsm->action(evt);
  } else
    Decoration::EventHandler(evt);
}

void Plug::EventHandler(XEvent* evt)
{
  if (evt->type == Expose) {
    if (evt->xexpose.count == 0)
      if (WLACTIVEP(graphic)) {
        WL_ACTIVE(graphic)->draw(this);
      } else
        XClearWindow(dpy, hook);
  } else if (evt->type == GWMExposeEvent) {
    if (WLACTIVEP(graphic)) {
      WL_ACTIVE(graphic)->draw(this);
    } else {
      if (WLPAINTP(graphic))
        WL_PAINT(graphic)->paint_background(hook, &box);
      if (box.shaped_back() || box.hole_parts())
        ReconfigureUp(0, 0, -1, 0, 0);
    }
  } else
    Decoration::EventHandler(evt);
}

void IMenu::EventHandler(XEvent* evt)
{
  if (evt->type == ClientMessage) {
    if (evt->xclient.message_type == XA_WM_PROTOCOLS
        && evt->xclient.data.l[0] == (long) XA_WM_DELETE_WINDOW) {
      window->UnDecorateWindow(0);
    } else {
//      printf("ClientMessage to menu: %lld\n", evt->xclient.data.l[0]);
      Decoration::EventHandler(evt);
      //window->Deco()->EventHandler(evt);
    }
  } else if (evt->type == GWMUserEvent || evt->type == GWMOpeningEvent || evt->type == GWMClosingEvent)
    menu->EventHandler(evt);
  else
    Decoration::EventHandler(evt);
}

void IClient::EventHandler(XEvent* evt)
{
  if (evt->type == ConfigureRequest) {
    window->ConfigureRequestEventHandler((XConfigureRequestEvent*) evt);
  } else if (evt->type == MapRequest) {     /* client is mapping its window */
    if (window)
      window->EventHandler(evt);
  } else
    Decoration::EventHandler(evt);
}

void UScreen::EventHandler(XEvent* evt)
{
  if (!screen->EventHandler(evt))
    Decoration::EventHandler(evt);
}

void UScreen::ResizeScreen(int w, int h)
{
  if (box.width != w || box.height != h) {
    box.width = screen->width;
    box.height = screen->height;
    issue_resize_event();
  }
}

void Decoration::ReconfigureTurn(int inhibrc)
{
  int xoff1, yoff1, xoff2, yoff2;
  int flag;
  if ((flag = (valid > 0 && window && Top() == window->Deco() && !window->Inner()->InhibitGravity())))
    window->CalcGravityOffset(xoff1, yoff1);
  setnaturalsize(0);
  if (parent && floating) {
    int x, y, w, h;
    interpret_anchor(adescr, &parent->box, &box, x, y, w, h);
    updatesize(w, h);
    updatepos(x, y);
  } else
    updatesize(-1, -1);
  if (flag) {
    window->CalcGravityOffset(xoff2, yoff2);
    if (xoff1 != xoff2 || yoff1 != yoff2)
      window->Deco()->changepos(window->Deco()->Xpos() - xoff2 + xoff1,
                                window->Deco()->Ypos() - yoff2 + yoff1);
  }
  if (valid > 0)
    if (inhibrc)
      DelayReconfigure(this);
    else
      ReconfigureDown();
}

void IMenu::ReconfigureTurn(int inhibrc)
{
  int xoff1, yoff1, xoff2, yoff2;
  int flag;
  if ((flag = (valid > 0 && !parent && window && Top() == window->Deco())))
    window->CalcGravityOffset(xoff1, yoff1);
  menu->setnaturalsize(0);
  menu->updatesize(-1, -1);
  updatesize(-1, -1);
  box.width = menu->Width() + 2*menu->Borderwidth();
  box.height = menu->Height() + 2*menu->Borderwidth();
  if (valid > 0) {
    ResizeWindow();
    if (flag) {
      window->CalcGravityOffset(xoff2, yoff2);
      if (xoff1 != xoff2 || yoff1 != yoff2)
        window->Deco()->changepos(window->Deco()->Xpos() - xoff2 + xoff1,
                                  window->Deco()->Ypos() - yoff2 + yoff1);
    }
    if (inhibrc)
      DelayReconfigure(menu);
    else
      menu->ReconfigureDown();
    if (menu->Shaped())
      internal_shape();
    else if (box.hole_parts())
      reset_shape();
  }
  box.set_hole_parts(menu->Shaped());
}
  
void Bar::ReconfigureUp(int size, int pos, int shape, int inhibsh, int inhibrc)
{
  int i, tpos = 0;
  if (pos == 1) {
    if (box.shaped_back() || box.hole_parts())
      tpos = 1;
    pos = 0;
  } else if (pos == -1)
    pos = 1;
  if (shape || tpos) {
    int osh = box.shaped();
    box.set_trans_parts(0);
    box.set_hole_parts(0);
    for (i = 0; i < nplugs; i++)
      if (plugs[i]) {
        if (plugs[i]->box.shaped_trans() && !plugs[i]->hidden)
          box.set_trans_parts(1);
        if (plugs[i]->box.shaped_hole() && !plugs[i]->hidden)
          box.set_hole_parts(1);
      }
    if (!size && !inhibsh) {
      shape = box.shaped() || osh || (shape == -1);
      if (shape && valid > 0)
        paint_shape();
    } else if (!box.shaped() && osh)
      shape_dirty = 1;
  }
  if (size) {
    if (floating || !parent) {
      ReconfigureTurn(inhibrc);
      size = 0;
    }
    if (!inhibsh)
      shape = box.shaped() || shape;
    if (floating)
      pos = 1;
  }
  if (shape == -1)
    shape = 1;
  if (size == -1)
    size = 1;
  if (parent && (size || pos || shape)) {
    parent->ReconfigureUp(size, pos, shape, inhibsh, inhibrc || delay_reconfigure);
  }
}

void Plug::ReconfigureUp(int size, int pos, int shape, int inhibsh, int inhibrc)
{
  if (pos == -1)
    pos = 1;
  if (shape) {
    if (!size && !inhibsh) {
      shape = box.shaped() || (shape == -1);
      if (shape && valid > 0)
        paint_shape();
    } else if (!box.shaped())
      shape_dirty = 1;
  }
  if (size) {
    if (floating || !parent) {
      ReconfigureTurn(inhibrc);
      size = 0;
    }
    if (!inhibsh)
      shape = box.shaped() || shape;
    if (floating)
      pos = 1;
  }
  if (shape == -1)
    shape = 1;
  if (size == -1)
    size = 1;
  if (parent && (size || pos || shape)) {
    parent->ReconfigureUp(size, pos, shape, inhibsh, inhibrc);
  }
}

void IClient::ReconfigureUp(int size, int pos, int shape, int inhibsh, int inhibrc)
{
  if (pos == -1)
    pos = 1;
  if (size) {
    if (floating || !parent) {
      ReconfigureTurn(inhibrc);
      size = 0;
    }
    if (floating)
      pos = 1;
  }
  if (shape == -1)
    shape = 1;
  if (size == -1)
    size = 1;
  if (parent && (size || pos || shape)) {
    parent->ReconfigureUp(size, pos, shape, inhibsh, inhibrc);
  }
}

void IMenu::ReconfigureUp(int size, int pos, int shape, int inhibsh, int inhibrc)
{
  XSizeHints* normal_hints;
  if (size)
    ReconfigureTurn(inhibrc);
  else if (shape) {
    if (menu->Shaped())
      internal_shape();
    else if (box.hole_parts())
      reset_shape();
    box.set_hole_parts(menu->Shaped());
  }
  if (valid > 0 && window) {
    normal_hints = window->NormalHints();
    normal_hints->min_width = normal_hints->max_width = menu->Width();
    normal_hints->min_height = normal_hints->max_height = menu->Height();
  }
  if (parent && (size || pos || shape))
    parent->ReconfigureUp(size, pos, shape, inhibsh, inhibrc);
}

void Bar::ReconfigureDown()
{
  int i;
  MoveResizeWindow();
  for (i = 0; i < nplugs; i++)
    if (plugs[i])
      plugs[i]->ReconfigureDown();
  if (box.shaped() || shape_dirty) {
    paint_shape();
    shape_dirty = 0;
  }
}

void Plug::ReconfigureDown()
{
  MoveResizeWindow();
  if (box.shaped() || shape_dirty) {
    paint_shape();
    shape_dirty = 0;
  }
}

void IMenu::ReconfigureDown()
{
  if (shape_dirty) {
    internal_shape();
    shape_dirty = 0;
  }
  MoveWindow();
}

void IClient::ReconfigureDown()
{
  MoveWindow();
  if (inner_pos_dirty) {
    SendSyntheticMoveEvent(inner_x+window->Deco()->box.x,
                           inner_y+window->Deco()->box.y);
    inner_pos_dirty = 0;
  }
}

void XXShapeCombineShapeInRect(Display* dpy, Window dest, int destKind,
                               int xOff, int yOff, XRectangle *rect,
                               Window src, int srcKind)
{
  static Window work = 0;
  if (!work) 
    work = XCreateSimpleWindow(dpy, Context->Root(), 0, 0, 1, 1, 0, 0, 0);
  XShapeCombineShape(dpy, work, destKind, 0, 0, dest, destKind, ShapeSet);
  XShapeCombineRectangles(dpy, work, destKind, xOff, yOff,
                          rect, 1, ShapeSubtract, 0); 
  XShapeCombineShape(dpy, work, destKind, xOff, yOff,
                     src, srcKind, ShapeUnion);
  XShapeCombineShape(dpy, dest, destKind, 0, 0, work, destKind, ShapeSet);
}

void Decoration::reset_shape()
{
  XShapeCombineMask(dpy, hook, ShapeBounding, 0, 0, None, ShapeSet); 
}

void Decoration::paint_shape()
{
  static Window work = 0;
  XRectangle rect;
  if (!box.shaped())
    reset_shape();
  else {
    if (!work) 
      work = XCreateSimpleWindow(dpy, Context->Root(), 0, 0, 1, 1, 0, 0, 0);
    rect.x = - box.borderwidth;
    rect.y = - box.borderwidth;
    rect.width = box.width + 2 * box.borderwidth;
    rect.height = box.height + 2 * box.borderwidth;
    paint_shape_intern(work, &rect);
    XShapeCombineShape(dpy, hook, ShapeBounding, 0, 0, work, ShapeBounding, ShapeSet);
  }
}

void Decoration::paint_shape_intern(Window work, XRectangle* r)
{
  XShapeCombineRectangles(dpy, work, ShapeBounding, 0, 0, r, 1, ShapeSet, 0); 
  if (WLPAINTP(background))
    WL_PAINT(background)->paint_background_shape(work, &box);
  if (box.borderwidth && WLPAINTP(bordercolor))
    WL_PAINT(bordercolor)->paint_border_shape(work, &box);
}

void Bar::paint_shape_intern(Window work, XRectangle* r)
{
  int i;
  XShapeCombineRectangles(dpy, work, ShapeBounding, 0, 0, r, 1, ShapeSet, 0); 
  if (WLPAINTP(background))
    WL_PAINT(background)->paint_background_shape(work, &box);
  for (i = 0; i < nplugs; i++)
    if (plugs[i]) {
      if ((box.shaped_back() || plugs[i]->box.shaped_hole() ||
           (plugs[i]->floating && box.hole_parts())) &&
          !plugs[i]->hidden)
        plugs[i]->inherit_shape(work);
    }
  XShapeCombineRectangles(dpy, work, ShapeBounding, 0, 0, r, 1, ShapeIntersect, 0); 
  if (box.borderwidth && WLPAINTP(bordercolor))
    WL_PAINT(bordercolor)->paint_border_shape(work, &box);
}

void Plug::paint_shape_intern(Window work, XRectangle* r)
{
  XShapeCombineRectangles(dpy, work, ShapeBounding, 0, 0, r, 1, ShapeSet, 0); 
  if (WLPIXMAPP(graphic))
    WL_PAINT(graphic)->paint_background_shape(work, &box);
  else if (WLACTIVEP(graphic))
    WL_ACTIVE(graphic)->paint_background_shape(work, this);
}

void Decoration::inherit_holes(Window hook, int xoff, int yoff)
{
  if (WLPAINTP(background))
    WL_PAINT(background)->paint_background_hole(hook, &box, xoff, yoff);
  if (WLPAINTP(bordercolor))
    WL_PAINT(bordercolor)->paint_border_hole(hook, &box, xoff, yoff);
}

void Bar::inherit_holes(Window hook, int xoff, int yoff)
{
  int i;
  if (WLPAINTP(background))
    WL_PAINT(background)->paint_background_hole(hook, &box, xoff, yoff);
  if (WLPAINTP(bordercolor))
    WL_PAINT(bordercolor)->paint_border_hole(hook, &box, xoff, yoff);
  if (box.shaped())
    for (i = 0; i < nplugs; i++)
      if (plugs[i] && !plugs[i]->hidden)
        plugs[i]->inherit_holes(hook,
                                xoff + plugs[i]->box.x + plugs[i]->box.borderwidth,
                                yoff + plugs[i]->box.y + plugs[i]->box.borderwidth);
}

void Plug::inherit_holes(Window hook, int xoff, int yoff)
{
  if (box.shaped())
    if (WLPIXMAPP(graphic))
      WL_PAINT(graphic)->paint_background_hole(hook, &box, xoff, yoff);
    else if (WLACTIVEP(graphic))
      WL_ACTIVE(graphic)->paint_background_hole(hook, this, xoff, yoff);
}

void IClient::inherit_holes(Window h, int xoff, int yoff)
{
  XRectangle rect;
  rect.x = 0;
  rect.y = 0;
  rect.width = box.width;
  rect.height = box.height;
  XXShapeCombineShapeInRect(dpy, h, ShapeBounding, xoff, yoff,
                            &rect, hook, ShapeBounding);
}

void Decoration::inherit_shape(Window h)
{
  XRectangle rect;
  int innerx = box.x + box.borderwidth;
  int innery = box.y + box.borderwidth;
  if (box.shaped_trans()) {
    if (box.shaped_hole())
      inherit_holes(h, innerx, innery);
    XShapeCombineShape(dpy, h, ShapeBounding, innerx, innery,
                       hook, ShapeBounding, ShapeUnion);
  } else if (box.shaped_hole()) {
    rect.x = - box.borderwidth;
    rect.y = - box.borderwidth;
    rect.width = box.width + 2 * box.borderwidth;
    rect.height = box.height + 2 * box.borderwidth;
    XXShapeCombineShapeInRect(dpy, h, ShapeBounding, innerx, innery,
                              &rect, hook, ShapeBounding);
  } else {
    rect.x = - box.borderwidth;
    rect.y = - box.borderwidth;
    rect.width = box.width + 2 * box.borderwidth;
    rect.height = box.height + 2 * box.borderwidth;
    XShapeCombineRectangles(dpy, h, ShapeBounding, innerx, innery,
                            &rect, 1, ShapeUnion, 0); 
  }
}

void IMenu::internal_shape()
{
  int bw = menu->Borderwidth();
  XShapeCombineShape(dpy, hook, ShapeBounding, bw, bw,
                     menu->Xwin(), ShapeBounding, ShapeSet);
}

void IClient::internal_shape()
{
  if (valid != 4)
    XShapeCombineShape(dpy, hook, ShapeBounding,
                       inner_borderwidth, inner_borderwidth,
                       wind, ShapeBounding, ShapeSet);
}

void IClient::change_shape(int s)
{
  int rubber = GWM_rubber_feedback && Visible();
  if (rubber) {
    UnDrawRubber();
    GWM_rubber_feedback = 0;
  }
  if (s) {
    internal_shape();
    box.set_hole_parts(1);
  } else {
    reset_shape();
    box.set_hole_parts(0);
  }
  ReconfigureUp(0, 0, -1, 0, 0);
  if (rubber) {
    ReDrawRubber();
    GWM_rubber_feedback = 1;
  }
}

void Plug::set_graphic(SCM gr)
{
  unsigned int w, h;
  int sh, sz, rubber;
  w = box.width;
  h = box.height;
  sh = box.shaped_back();
  if (WLPIXMAPP(graphic))
    WL_PIXMAP(graphic)->unregister_use(this, 0);
  else if (WLACTIVEP(graphic))
    WL_ACTIVE(graphic)->unregister_use(this);
  graphic = gr;
  box.shape_flags = 0;
  if (WLPIXMAPP(graphic)) {
    WL_PIXMAP(graphic)->register_use(this, 0);
    box.set_trans_back(WL_PAINT(graphic)->IsShapedT());
    box.set_hole_back(WL_PAINT(graphic)->IsShapedH());
  } else if (WLACTIVEP(graphic))
    WL_ACTIVE(graphic)->register_use(this);
  rubber = GWM_rubber_feedback && Visible();
  if (rubber) {
    UnDrawRubber();
    GWM_rubber_feedback = 0;
  }
  if (valid > 0) {
    if (WLPIXMAPP(graphic))
      WL_PAINT(graphic)->paint_background(hook, &box);
    else if (WLACTIVEP(graphic))
      WL_ACTIVE(graphic)->draw(this);
  }
  sh = (sh || box.shaped_back() ? -1 : 0);
  setnaturalsize(0);
  sz = (w != box.width || h != box.height ? -1 : 0);
  if (sz || sh)
    ReconfigureUp(sz, 0, sh, 0, 0);
  if (rubber) {
    ReDrawRubber();
    GWM_rubber_feedback = 1;
  }
}

SCM Bar::get_parts()
{
  int i;
  SCM res = SCM_EOL;
  for (i=nplugs-1; i>=0; i--)
    res = scm_cons((plugs[i] ? plugs[i]->scm() : SCM_EOL), res);
  return res;
}

SCM Bar::get_part(int n)
{
  if (n<=nplugs && n>0) {
    if (plugs[n-1])
      return plugs[n-1]->scm();
    else
      return SCM_EOL;
  } else
    return SCM_BOOL_F;
}

int Bar::get_num_parts()
{
  return nplugs;
}

void Bar::maintain_stacking(int n)
{
  XWindowChanges values;
  int i;
  if (plugs[n]->floating) {
    for (i=n-1; i>-1 && (!plugs[i] || !plugs[i]->floating); i--);
    if (i==-1)
      for (i=nplugs-1; i>-1 && (!plugs[i] || plugs[i]->floating); i--);
  } else
    for (i=n-1; i>-1 && (!plugs[i] || plugs[i]->floating); i--);
  if (i > -1) {
    values.sibling = plugs[i]->Xwin();
    values.stack_mode = Above;
    XConfigureWindow(dpy, plugs[n]->Xwin(), CWStackMode | CWSibling, &values);
  } else {
    if (plugs[n]->floating)
      for (i=n+1; i<nplugs && !plugs[i]; i++);
    else {
      for (i=n+1; i<nplugs && (!plugs[i] || plugs[i]->floating); i++);
      if (i==nplugs)
        for (i=0; i<nplugs && (!plugs[i] || !plugs[i]->floating); i++);
    }
    if (i < nplugs) {
      values.sibling = plugs[i]->Xwin();
      values.stack_mode = Below;
      XConfigureWindow(dpy, plugs[n]->Xwin(), CWStackMode | CWSibling, &values);
    }
  }
}

void Bar::set_part(SCM obj, int n)
{
  Decoration *newp, *old;
  int sh = 0, vis, rubber;
  if (WLPIXMAPP(obj) || WLACTIVEP(obj)) {
    if (n<=nplugs && n>0 && plugs[n-1] && !WLDECOP(plugs[n-1]->scm())) {
      ((Plug*) plugs[n-1])->set_graphic(obj);
      return;
    }
    newp = new Plug(obj);
    newp->parent = this;
    if (newp->box.shaped()) sh = -1;
  } else if (WLDECOP(obj)) {
    newp = WL_DECO(obj);
    if (newp->check_free_menu())
      newp = ((IMenu*)newp)->unrealize();
    newp->parent = this;
    if (newp->box.shaped()) sh = -1;
  } else
    newp = NULL;
  if (n<=nplugs && n>0) {
    old = plugs[n-1];
    plugs[n-1] = newp;
    vis = Visible();
    rubber = GWM_rubber_feedback && vis;
    if (rubber) {
      UnDrawRubber();
      GWM_rubber_feedback = 0;
    }
    if (old) {
      old->parent = 0;
      old->updatepos(0, 0);
      if (old->box.shaped()) sh = -1;
      if (old->valid > 0) {
        if (vis)
          old->issue_unmap_event();
        ProcessGwmEvents();
        old->execclose();
        old->close(0);
      } else if (old->valid < 0)
        old->close(0);
      old->ReconfigureTurn(0);
    }
    if (valid > 0) {
      if (newp) {
        if (!newp->open(status, window, screen)) {
          plugs[n-1] = NULL;
          gwm_warning("Failed to open deco: ~A", newp->scm());
          ReconfigureUp(1, 0, sh, 0, 0);
        } else {
          maintain_stacking(n-1);
          ReconfigureUp(1, 0, sh, 0, 0);
          newp->execopen();
          if (vis)
            newp->issue_map_event();
        }
      } else
        ReconfigureUp(1, 0, sh, 0, 0);
    } else {
      if (newp && valid < 0)
        newp->semiopen(window, screen);
      ReconfigureUp(1, 0, sh, 0, 0);
    }
    if (rubber) {
      ReDrawRubber();
      GWM_rubber_feedback = 1;
    }
  }
}

void Bar::insert_part(SCM obj, int n)
{
  Decoration** newplugs;
  Decoration *newp;
  int i, sh = 0, vis, rubber;
  if (WLPIXMAPP(obj) || WLACTIVEP(obj)) {
    newp = new Plug(obj);
    newp->parent = this;
    if (newp->box.shaped()) sh = -1;
  } else if (WLDECOP(obj)) {
    newp = WL_DECO(obj);
    if (newp->check_free_menu())
      newp = ((IMenu*)newp)->unrealize();
    newp->parent = this;
    if (newp->box.shaped()) sh = -1;
  } else
    newp = NULL;
  if (n<=nplugs+1 && n>0) {
    nplugs++;
    newplugs = new Decoration*[nplugs];
    for (i=0; i<n-1; i++) newplugs[i] = plugs[i];
    newplugs[i++] = newp;
    for (; i<nplugs; i++) newplugs[i] = plugs[i-1];
    delete [] plugs;
    plugs = newplugs;
    if (valid > 0) {
      vis = Visible();
      rubber = (vis && GWM_rubber_feedback);
      if (rubber) {
        UnDrawRubber();
        GWM_rubber_feedback = 0;
      }
      if (newp) {
        if (!newp->open(status, window, screen)) {
          plugs[n-1] = NULL;
          gwm_warning("Failed to open deco: ~A", newp->scm());
          ReconfigureUp(1, 0, sh, 0, 0);
        } else {
          maintain_stacking(n-1);
          ReconfigureUp(1, 0, sh, 0, 0);
          newp->execopen();
          if (vis)
            newp->issue_map_event();
        }
      } else
        ReconfigureUp(1, 0, sh, 0, 0);
      if (rubber) {
        ReDrawRubber();
        GWM_rubber_feedback = 1;
      }
    } else {
      if (newp && valid < 0)
        newp->semiopen(window, screen);
      ReconfigureUp(1, 0, sh, 0, 0);
    }
  }
}

void Bar::remove_part(int n)
{
  Decoration** newplugs;
  Decoration *old;
  int i, sh = 0, vis, rubber;
  if (n<=nplugs && n>0) {
    old = plugs[n-1];
    nplugs--;
    newplugs = new Decoration*[nplugs];
    for (i=0; i<n-1; i++) newplugs[i] = plugs[i];
    for (; i<nplugs; i++) newplugs[i] = plugs[i+1];
    delete [] plugs;
    plugs = newplugs;
    vis = Visible();
    rubber = (vis && GWM_rubber_feedback);
    if (rubber) {
      UnDrawRubber();
      GWM_rubber_feedback = 0;
    }
    if (old) {
      old->parent = 0;
      old->updatepos(0, 0);
      if (old->box.shaped()) sh = -1;
      if (old->valid > 0) {
        if (vis)
          old->issue_unmap_event();
        ProcessGwmEvents();
        old->execclose();
        old->close(0);
      } else if (old->valid < 0)
        old->close(0);
      old->ReconfigureTurn(0);
    }
    ReconfigureUp(1, 0, sh, 0, 0);
    if (rubber) {
      ReDrawRubber();
      GWM_rubber_feedback = 1;
    }
  }
}

void Bar::remove_part(Decoration* part)
{
  int i;
  for (i=0; i<nplugs; i++)
    if (plugs[i] == part) {
      remove_part(i+1);
      return;
    }
}

void Bar::reorder_part(int n, int m)
{
  Decoration* old;
  int fl = 0;
  int i, rubber;
  if (n<=nplugs && n>0 && m<=nplugs && m>0 && n != m) {
    old = plugs[n-1];
    if (n<m) {
      for (i=n-1; i<m-1; i++) {
        plugs[i] = plugs[i+1];
        if (plugs[i] && plugs[i]->floating) fl = 1;
      }
    } else {
      for (i=n-1; i>m-1; i--) {
        plugs[i] = plugs[i-1];
        if (plugs[i] && plugs[i]->floating) fl = 1;
      }
    }
    plugs[m-1] = old;
    if (valid > 0) {
      rubber = (Visible() && GWM_rubber_feedback);
      if (rubber) {
        UnDrawRubber();
        GWM_rubber_feedback = 0;
      }
      if (old) {
        maintain_stacking(m-1);
        if (old->floating && fl)
          issue_stack_event();
      }
      ReconfigureUp(1, 0, 0, 0, 0);
      if (rubber) {
        ReDrawRubber();
        GWM_rubber_feedback = 1;
      }
    } else
      ReconfigureUp(1, 0, 0, 0, 0);
  }
}

void IClient::Resize(int w, int h)
{
  int rubber;
  if (Fsm::ServerGrabbed())
    DelayResizeClient(this, w, h);
  else
    XResizeWindow(dpy, wind, w, h);
  box.width = w + 2*inner_borderwidth;
  box.height = h + 2*inner_borderwidth;
  rubber = (GWM_rubber_feedback && Visible());
  if (rubber) {
    UnDrawRubber();
    GWM_rubber_feedback = 0;
  }
  ResizeWindow();
  inhibit_gravity = 0;
  ReconfigureUp(-1, 0, 0, 1, 0);
  if (rubber) {
    ReDrawRubber();
    GWM_rubber_feedback = 1;
  }
}

void IClient::MoveResize(int x, int y, int w, int h)
{
  int rubber;
  if (Fsm::ServerGrabbed())
    DelayResizeClient(this, w, h);
  else
    XResizeWindow(dpy, wind, w, h);
  box.width = w + 2*inner_borderwidth;
  box.height = h + 2*inner_borderwidth;
  rubber = (GWM_rubber_feedback && Visible());
  if (rubber) {
    UnDrawRubber();
    GWM_rubber_feedback = 0;
  }
  ResizeWindow();
  Top()->updatepos(x - inner_x + inner_borderwidth,
                   y - inner_y + inner_borderwidth);
  Top()->MoveWindow();
  inhibit_gravity = 1;
  ReconfigureUp(-1, 0, 0, 1, 0);
  inhibit_gravity = 0;
  if (rubber) {
    ReDrawRubber();
    GWM_rubber_feedback = 1;
  }
}

void IClient::ReconfBorderwidth(int bw)
{
  orig_borderwidth = bw;
  if (borderwidth == -1) {
    inner_borderwidth = orig_borderwidth;
    XSetWindowBorderWidth(dpy, wind, inner_borderwidth);
  }
}

void IClient::ConsiderClientSize(CachedProperties* props, int new_win)
{
  unsigned int bw, width, height, owidth, oheight;
  int must_resize = 0;
  /* update the size according to hints */
//  printf("  orig: x=%d, y=%d, width=%d, height=%d, bw=%d\n", orig_x, orig_y, box.width, box.height, orig_borderwidth);
  width = box.width - 2 * orig_borderwidth;
  height = box.height - 2 * orig_borderwidth;
  bw = orig_borderwidth;
  if (new_win) {
    owidth = width;
    oheight = height;
    conform_to_hints(&(props->normal_hints), (int*)&width, (int*)&height);
    if ((owidth != width) ||
        (oheight != height)) {
      must_resize = 1;
    }
  }
  if (must_resize) {
    box.width = width + 2*bw;
    box.height = height + 2*bw;
    XResizeWindow(dpy, wind, width, height);
    if (Valid() > 0)
      ResizeWindow();
  }
//  printf("  result: x=%d, y=%d, width=%d, height=%d, bw=%d\n", orig_x, orig_y, box.width, box.height, orig_borderwidth);
}

/* Used to tell a client that it has been moved */
void IClient::SendSyntheticMoveEvent(int x, int y)
{
  int w, h;
  XConfigureEvent event;
  if (valid == 4)
    return;
  event.type = ConfigureNotify;
  event.display = dpy;
  event.event = wind;
  event.window = wind;
  event.x = x;
  event.y = y;
  if (Fsm::ServerGrabbed() && DelayedClientSize(this, w, h)) {
    event.width = w;
    event.height = h;
  } else {
    event.width = box.width - 2*inner_borderwidth;
    event.height = box.height - 2*inner_borderwidth;
  }
  event.border_width = inner_borderwidth;
  event.above = None;
  event.override_redirect = False;
  XSendEvent(dpy, wind, False, StructureNotifyMask, (XEvent*) &event);
}

void IClient::GetInnerDims(int& ulx, int& uly, int& lrx, int& lry)
{
  ulx = inner_x;
  uly = inner_y;
  lrx = inner_x + box.width - 2*inner_borderwidth - 1;
  lry = inner_y + box.height - 2*inner_borderwidth - 1;
}

void IMenu::GetInnerDims(int& ulx, int& uly, int& lrx, int& lry)
{
  ulx = inner_x;
  uly = inner_y;
  lrx = inner_x + box.width - 1;
  lry = inner_y + box.height - 1;
}

void Decoration::set_active_grab(Decoration* par, WlCursor* cursor, unsigned int specmask, int grab_kbd, int grab_cld, int grab_cnf, int grab_nofrz, int async)
{
  MaybeMenu()->fsm->SetGrab((par ? par->MaybeMenu() : 0),
                            specmask, grab_kbd, grab_cld, grab_cnf, 
                            grab_nofrz, async, cursor);
}

void Decoration::remove_active_grab(int async)
{
  MaybeMenu()->fsm->RemoveGrab(async);
}

void init_scm_deco()
{
  scm_tc16_deco = scm_make_smob_type("deco", 0);
  scm_set_smob_mark(scm_tc16_deco, mark_deco);
  scm_set_smob_free(scm_tc16_deco, free_deco);
  scm_set_smob_print(scm_tc16_deco, print_deco);
}

