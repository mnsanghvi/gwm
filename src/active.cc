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


 /************************\
 * 		          *
 *  Active pixmap object  *
 * 		          *
 \************************/

#include <guile/gh.h>

#include "gwm.hh"
#include "gwmfunc.hh"
#include "error.hh"
#include "deco.hh"
#include "screen.hh"
#include "paint.hh"
#include "font.hh"
#include "event.hh"
#include "drawing.hh"
#include "active.hh"

/*
extern long scm_tc16_wlactive;

class WlActive {
public:
  WlActive(SCM str, SCM ctx, int cn);
  ~WlActive();
  void dimensions(struct Box* box);
  void intrinsic_dimensions(struct Box* box);
  void draw(class Decoration* wob);
  void setup_shape(int hole);
  void register_use(class Decoration* deco);
  void unregister_use(class Decoration* deco);
  void issue_expose_users();
  void mark_dirty();
  void mark_dirty_size();
  void mark_dirty_one(class Decoration* deco);
  SCM get_background() { return background; };
  SCM get_function() { return drawfunc; };
  void set_background(SCM paint) { if (receptive) return; background = paint; mark_dirty(); };
  void set_function(SCM func) { if (receptive) return; drawfunc = func; mark_dirty(); };
  void set_size(int w, int h) { if (receptive) return; width = w; height = h; mark_dirty_size(); };
  void paint_background_shape(Window hook, class Decoration* deco);
  void paint_background_hole(Window hook, class Decoration* deco, int xoff, int yoff);
  void add_point(int x, int y, SCM p);
  void add_line(int x1, int y1, int x2, int y2, SCM p);
  void add_polygon(int np, XPoint* pts, SCM p);
  void add_rect(int x, int y, int w, int h, int b, SCM p);
  void add_circle(int x, int y, int w, int h, int b, int a1, int a2, SCM p);
  void add_text(int x, int y, SCM f, char* t, SCM p);
  void add_bitmap(int x, int y, int w, int h, Pixmap m, SCM p);
  Pixmap& get_transmask();
  Pixmap& get_holemask();
  SCM drawfunc;
  SCM background;
  int width;
  int height;
  Pixmap transmask;
  Pixmap holemask;
  short receptive;
  short draw_directly;
  short dirty;
  class Drawing* drawing;
  class DecoListA* usedby;
  class DecoListA* curr;
  SCM self;
};

SCM make_active(SCM arg1, SCM arg2);

#define WLACTIVEP(x) (SCM_NIMP(x) && SCM_CAR(x) == (SCM)scm_tc16_wlactive)
#define WL_ACTIVE(x) ((WlActive*) SCM_CDR(x))

*/

class DecoListA {
public:
  DecoListA(Decoration* d) { deco = d; drawing = 0; transmask = 0; holemask = 0; dirty = 0; shaped = 0; resized = 0; next = 0; };
  ~DecoListA() { if (drawing) delete drawing; if (transmask) XFreePixmap(dpy, transmask); if (holemask) XFreePixmap(dpy, holemask); };
  Decoration* deco;
  Drawing* drawing;
  Pixmap transmask;
  Pixmap holemask;
  short dirty;
  short resized;
  short shaped;
  DecoListA* next;
};

long scm_tc16_wlactive;

SCM mark_wlactive(SCM obj)
{
  WlActive* active = WL_ACTIVE(obj);
  if (active->background) scm_gc_mark(active->background);
  if (active->drawfunc) scm_gc_mark(active->drawfunc);
  return SCM_BOOL_F;
}

size_t free_wlactive(SCM obj)
{
  delete WL_ACTIVE(obj);
  return 0;
};

int print_wlactive(SCM obj, SCM port, scm_print_state * pstate)
{
  WlActive* act = WL_ACTIVE(obj);
  scm_puts("#<active pixmap: ", port);
  if (act->width == -1)
    scm_puts("?", port);
  else
    scm_write(gh_int2scm(act->width), port);
  scm_puts("x", port);
  if (act->height == -1)
    scm_puts("?", port);
  else
    scm_write(gh_int2scm(act->height), port);
  scm_puts(">", port);
  return 1;
};


SCM wlactive2scm(WlActive* active)
{
  return scm_cell((scm_t_bits) scm_tc16_wlactive, (scm_t_bits) active);
}

SCM_DEFINE(wl_active_p, "active-pixmap?", 1, 0, 0,
           (SCM obj),
           "Return true if obj is an active pixmap.")
{
  return (WLACTIVEP(obj) ? SCM_BOOL_T : SCM_BOOL_F);
}


SCM_DEFINE(make_active, "make-active-pixmap", 1, 0, 1,
           (SCM func, SCM args),
           "Make an active pixmap with a drawing function. Possible keys are:"
           ":background"
           ":width"
           ":height"
           ":cached")
{
  WlActive* object;
  SCM sub, ctx;
  int n, cn;
  if ((n = wl_separate_context(args, sub, ctx, cn, s_make_active)) != 0)
    gwm_wrong_num_args(s_make_active, n+1);
  if (!gh_procedure_p(func))
    gwm_wrong_type_arg(s_make_active, 1, func, "procedure");
  object = new WlActive(func, ctx, cn);
  return object->self;
}

SCM_DEFINE(refresh_active, "refresh-active", 1, 0, 0,
           (SCM active),
           "Force a redraw of the active pixmap, causing its draw function to be called.")
{
  if (!WLACTIVEP(active))
    gwm_wrong_type_arg(s_refresh_active, 1, active, "active pixmap");
  WL_ACTIVE(active)->mark_dirty();
  return SCM_UNSPECIFIED;
}

SCM_DEFINE(active_function, "active-function", 1, 0, 0,
           (SCM active),
           "Return the draw function for the active pixmap.")
{
  if (!WLACTIVEP(active))
    gwm_wrong_type_arg(s_active_function, 1, active, "active pixmap");
  return WL_ACTIVE(active)->get_function();
}

SCM_DEFINE(set_active_function, "set-active-function!", 2, 0, 0,
           (SCM active, SCM func),
           "Set the draw function for the active pixmap.")
{
  if (!WLACTIVEP(active))
    gwm_wrong_type_arg(s_set_active_function, 1, active, "active pixmap");
  if (!gh_procedure_p(func))
    gwm_wrong_type_arg(s_set_active_function, 2, func, "procedure");
  WL_ACTIVE(active)->set_function(func);
  return SCM_UNSPECIFIED;
}

SCM_DEFINE(active_background, "active-background", 2, 0, 0,
           (SCM active, SCM bg),
           "Return the background of the active pixmap.")
{
  if (!WLACTIVEP(active))
    gwm_wrong_type_arg(s_active_background, 1, active, "active pixmap");
  return WL_ACTIVE(active)->get_background();
}

SCM_DEFINE(set_active_background, "set-active-background!", 2, 0, 0,
           (SCM active, SCM bg),
           "Set the background of the active pixmap.")
{
  if (!WLACTIVEP(active))
    gwm_wrong_type_arg(s_set_active_background, 1, active, "active pixmap");
  if (!WLPAINTP(bg))
    gwm_wrong_type_arg(s_set_active_background, 2, bg, "color or pixmap");
  WL_ACTIVE(active)->set_background(bg);
  return SCM_UNSPECIFIED;
}

SCM_DEFINE(set_active_size, "set-active-size!", 3, 0, 0,
           (SCM active, SCM w, SCM h),
           "Set the size of the active pixmap.")
{
  int width, height;
  if (!WLACTIVEP(active))
    gwm_wrong_type_arg(s_set_active_size, 1, active, "active pixmap");
  width = wl_getposint(w, s_set_active_size, 2);
  height = wl_getposint(h, s_set_active_size, 3);
  WL_ACTIVE(active)->set_size(width, height);
  return SCM_UNSPECIFIED;
}

WlActive::WlActive(SCM func, SCM ctx, int cn)
{
  drawfunc = func;
  background = gwm_get_keyword(k_background, ctx, cn, SCM_UNDEFINED);
  if (background == SCM_UNDEFINED)
    background = Context->pixel.White;
  else if (!WLPAINTP(background))
    gwm_wrong_type_arg(s_make_active, 0, background, "color or pixmap");
  width = wl_getposint(gwm_get_keyword(k_width, ctx, cn, SCM_BOOL_F), s_make_active); 
  height = wl_getposint(gwm_get_keyword(k_height, ctx, cn, SCM_BOOL_F), s_make_active); 
  draw_directly = !wl_getbool(gwm_get_keyword(k_cached, ctx, cn, SCM_BOOL_T), s_make_active);
  if (width != -1 && height != -1) {
    drawing = new Drawing();
    dirty = 1;
  } else {
    drawing = 0;
    dirty = 0;
  }
  transmask = 0;
  holemask = 0;
  receptive = 0;
  usedby = 0;
  self = wlactive2scm(this);
}

WlActive::~WlActive()
{
  DecoListA* p;
  if (transmask)
    XFreePixmap(dpy, transmask);
  if (holemask)
    XFreePixmap(dpy, holemask);
  if (drawing)
    delete drawing;
  while (usedby) {
    p = usedby;
    usedby = usedby->next;
    delete p;
  }
}

Pixmap& WlActive::get_transmask()
{
  return (drawing ? transmask : curr->transmask); 
}

Pixmap& WlActive::get_holemask()
{
  return (drawing ? holemask : curr->holemask);
}

void WlActive::dimensions(Box* box)
{
  if (!curr) 
    intrinsic_dimensions(box);
  else {
    box->x = box->y = 0;
    box->width = curr->deco->Width();
    box->height = curr->deco->Height();
  }
}

void WlActive::intrinsic_dimensions(Box* box)
{
  box->x = box->y = 0;
  box->width = (width == -1 ? 0 : width);
  box->height = (height == -1 ? 0 : height);
}

void WlActive::draw(Decoration* wob)
{
  GC gc;
  DecoListA* p;
  XEvent dummy;
  if (receptive) return; // Someone is drawing already
  for (p=usedby; p && p->deco != wob; p=p->next);
  if (!p) return; // Window not handled by this active
  curr = p;
  gc = wob->Screen()->gc.Work;
  if (curr->resized) {
    wob->ReconfigureUp(-1, 0, 0, 0, 0);
    curr->resized = 0;
  }
  if (draw_directly) {
    while (XCheckWindowEvent(dpy, wob->Xwin(), ExposureMask, &dummy));
    if (curr->shaped) {  // Remove shape
      wob->GetBox()->set_trans_back(0);
      wob->GetBox()->set_hole_back(0);
      wob->ReconfigureUp(0, 0, -1, 0, 0);
      curr->shaped = 0;
      XSync(dpy, 0);
      while (XCheckWindowEvent(dpy, wob->Xwin(), ExposureMask, &dummy));
    } 
    receptive = 1;
    WL_PAINT(background)->paint_background(wob->Xwin(), wob->GetBox());
    WL_PAINT(background)->draw_rect_sh(this, 0, 0, wob->Width(), wob->Height(), 0);
    scm_apply(drawfunc, SCM_LIST1(self), SCM_EOL);
    receptive = 0;
    wob->GetBox()->set_trans_back(get_transmask());
    wob->GetBox()->set_hole_back(get_holemask());
    if (get_transmask() || get_holemask()) {  // Redraw shape
      curr->shaped = 1;
      wob->ReconfigureUp(0, 0, -1, 0, 0);
    }
  } else {
    if (drawing ? dirty : curr->dirty) {
      dirty = 0;
      curr->dirty = 0;
      WL_PAINT(background)->draw_rect_sh(this, 0, 0, wob->Width(), wob->Height(), 0);
      receptive = 1;
      scm_apply(drawfunc, SCM_LIST1(self), SCM_EOL);
      receptive = 0;
      WL_PAINT(background)->paint_background(wob->Xwin(), wob->GetBox());
      wob->GetBox()->set_trans_back(get_transmask());
      wob->GetBox()->set_hole_back(get_holemask());
      XSync(dpy, 0);
      while (XCheckWindowEvent(dpy, wob->Xwin(), ExposureMask, &dummy));
    } else if (curr->dirty == -1) {  // Newcommer
      curr->dirty = 0;
      WL_PAINT(background)->paint_background(wob->Xwin(), wob->GetBox());
      wob->GetBox()->set_trans_back(get_transmask());
      wob->GetBox()->set_hole_back(get_holemask());
    }
    if (get_transmask() || get_holemask() || wob->Shaped()) {
      wob->ReconfigureUp(0, 0, -1, 0, 0);
      curr->shaped = 1;
    } else if (curr->shaped) {
      wob->ReconfigureUp(0, 0, -1, 0, 0);
      curr->shaped = 0;
    }
    XClearWindow(dpy, wob->Xwin());
    if (drawing)
      drawing->Draw(wob->Xwin());
    else
      curr->drawing->Draw(wob->Xwin());
  }
  curr = 0;
}

void WlActive::setup_shape(int hole)
{
  Pixmap p;
  int w, h;
  if (hole ? !get_holemask() : !get_transmask()) {
    w = (width == -1 ? curr->deco->Width() : width);
    if (w == 0) w = 1;
    h = (height == -1 ? curr->deco->Height() : height);
    if (h == 0) h = 1;
    p = XCreatePixmap(dpy, Context->Root(), w, h, 1);
    XSetForeground(dpy, Context->gc.Shape, 0);
    XFillRectangle(dpy, p, Context->gc.Shape, 0, 0, w, h);
    if (hole)
      get_holemask() = p;
    else
      get_transmask() = p;
  }
}

void WlActive::register_use(class Decoration* deco)
{
  DecoListA* ele = new DecoListA(deco);
  if (!drawing) {
    ele->dirty = 1;
    ele->drawing = new Drawing();
  } else
    ele->dirty = -1;
  ele->next = usedby;
  usedby = ele;
}

void WlActive::unregister_use(class Decoration* deco)
{
  DecoListA *ele = usedby, *ele2 = 0;
  while (ele && ele->deco != deco)
    ele2 = ele, ele = ele->next;
  if (ele) {
    if (ele2)
      ele2->next = ele->next;
    else
      usedby = ele->next;
    delete ele;
  }
}

void WlActive::issue_expose_users()
{
  DecoListA *ele = usedby;
  while (ele) {
    if (ele->deco->Valid() > 0)
      send_gwm_event(ele->deco, GWMExposeEvent);
    ele = ele->next;
  }
}

void WlActive::mark_dirty()
{
  DecoListA *ele = usedby;
  if (receptive) return; 
  if (drawing) {
    dirty = 1;
    drawing->Clear();
    if (transmask)
      XFreePixmap(dpy, transmask);
    if (holemask)
      XFreePixmap(dpy, holemask);
    transmask = 0;
    holemask = 0;
    while (ele) {
      ele->dirty = -1;
      ele = ele->next;
    }
  } else {
    while (ele) {
      ele->dirty = 1;
      ele->drawing->Clear();
      if (ele->transmask)
        XFreePixmap(dpy, ele->transmask);
      if (ele->holemask)
        XFreePixmap(dpy, ele->holemask);
      ele->transmask = 0;
      ele->holemask = 0;
      ele = ele->next;
    }
  }
  issue_expose_users();
}

void WlActive::mark_dirty_size()
{
  DecoListA *ele = usedby;
  if (drawing) {
    if (width == -1 || height == -1) {
      dirty = 0;
      delete drawing;
      drawing = 0;
      if (transmask)
        XFreePixmap(dpy, transmask);
      if (holemask)
        XFreePixmap(dpy, holemask);
      transmask = 0;
      holemask = 0;
      while (ele) {
        ele->drawing = new Drawing();
        ele->dirty = 1;
        ele->resized = 1;
        ele = ele->next;
      }
    } else {
      dirty = 1;
      drawing->Clear();
      if (transmask)
        XFreePixmap(dpy, transmask);
      if (holemask)
        XFreePixmap(dpy, holemask);
      transmask = 0;
      holemask = 0;
      while (ele) {
        ele->dirty = -1;
        ele->resized = 1;
        ele = ele->next;
      }
    }
  } else {
    if (width != -1 && height != -1) {
      while (ele) {
        delete ele->drawing;
        ele->dirty = -1;
        ele->drawing = 0;
        if (ele->transmask)
          XFreePixmap(dpy, ele->transmask);
        if (ele->holemask)
          XFreePixmap(dpy, ele->holemask);
        ele->transmask = 0;
        ele->holemask = 0;
        ele->resized = 1;
        ele = ele->next;
      }
      dirty = 1;
      drawing = new Drawing();
    } else {
      while (ele) {
        ele->resized = 1;
        ele = ele->next;
      }
    }
  }
  issue_expose_users();
}

void WlActive::mark_dirty_one(class Decoration* deco)
{
  DecoListA* ele;
  if (receptive) receptive = 0;
  for (ele=usedby; ele && ele->deco != deco; ele=ele->next);
  if (!ele || drawing) return;
  ele->drawing->Clear();
  if (ele->transmask)
    XFreePixmap(dpy, ele->transmask);
  if (ele->holemask)
    XFreePixmap(dpy, ele->holemask);
  ele->transmask = 0;
  ele->holemask = 0;
  ele->dirty = 1;
  if (ele->deco->Valid() > 0)
    send_gwm_event(ele->deco, GWMExposeEvent);
}

void WlActive::paint_background_shape(Window hook, Decoration* deco)
{
  DecoListA* p;
  int ok=1;
  for (p=usedby; p && p->deco != deco; p=p->next);

  if (deco->GetBox()->nonzerosize()) {
    if (drawing ? transmask : p->transmask) {
      XShapeCombineMask(dpy, hook, ShapeBounding, 0, 0,
                        (drawing ? transmask : p->transmask), ShapeSubtract);
      ok = 0;
    }
    if (drawing ? holemask : p->holemask) {
      XShapeCombineMask(dpy, hook, ShapeBounding, 0, 0,
                        (drawing ? holemask : p->holemask), ShapeSubtract);
      ok = 0;
    }
    if (ok)
      XShapeCombineMask(dpy, hook, ShapeBounding, 0, 0, None, ShapeSet); 
  } 
}

void WlActive::paint_background_hole(Window hook, Decoration* deco, int xoff, int yoff)
{
  DecoListA* p;
  for (p=usedby; p && p->deco != deco; p=p->next);
  if ((drawing ? holemask : p->holemask) && deco->GetBox()->nonzerosize())
    XShapeCombineMask(dpy, hook, ShapeBounding, xoff, yoff,
                      (drawing ? holemask : p->holemask), ShapeSubtract);
}

void WlActive::add_point(int x, int y, SCM p)
{
  if (!receptive) return;
  if (draw_directly)
    WL_PAINT(p)->draw_point(curr->deco->Xwin(), x, y);
  else
    (drawing ? drawing : curr->drawing)->Add(new DrawingPoint(x, y, p));
  WL_PAINT(p)->draw_point_sh(this, x, y);
}

void WlActive::add_line(int x1, int y1, int x2, int y2, SCM p)
{
  if (!receptive) return;
  if (draw_directly)
    WL_PAINT(p)->draw_line(curr->deco->Xwin(), x1, y1, x2, y2);
  else
    (drawing ? drawing : curr->drawing)->Add(new DrawingLine(x1, y1, x2, y2, p));
  WL_PAINT(p)->draw_line_sh(this, x1, y1, x2, y2);
}

void WlActive::add_polygon(int np, XPoint* pts, SCM p)
{
  if (!receptive) return;
  if (draw_directly)
    WL_PAINT(p)->draw_polygon(curr->deco->Xwin(), np, pts);
  else
    (drawing ? drawing : curr->drawing)->Add(new DrawingPolygon(np, pts, p));
  WL_PAINT(p)->draw_polygon_sh(this, np, pts);
}

void WlActive::add_rect(int x, int y, int w, int h, int b, SCM p)
{
  if (!receptive) return;
  if (draw_directly)
    WL_PAINT(p)->draw_rect(curr->deco->Xwin(), x, y, w, h, b);
  else
    (drawing ? drawing : curr->drawing)->Add(new DrawingRect(x, y, w, h, b, p));
  WL_PAINT(p)->draw_rect_sh(this, x, y, w, h, b);
}

void WlActive::add_circle(int x, int y, int w, int h, int b, int a1, int a2, SCM p)
{
  if (!receptive) return;
  if (draw_directly)
    WL_PAINT(p)->draw_circle(curr->deco->Xwin(), x, y, w, h, b, a1, a2);
  else
    (drawing ? drawing : curr->drawing)->Add(new DrawingCircle(x, y, w, h, b, a1, a2, p));
  WL_PAINT(p)->draw_circle_sh(this, x, y, w, h, b, a1, a2);
}

void WlActive::add_text(int x, int y, SCM f, char* t, SCM p)
{
  if (!receptive) return;
  if (draw_directly)
    WL_PAINT(p)->draw_text(curr->deco->Xwin(), x, y, WL_FONT(f), t);
  else
    (drawing ? drawing : curr->drawing)->Add(new DrawingText(x, y, f, t, p));
  WL_PAINT(p)->draw_text_sh(this, x, y, WL_FONT(f), t);
}

void WlActive::add_bitmap(int x, int y, int w, int h, Pixmap m, SCM p)
{
  if (!receptive) return;
  if (draw_directly)
    WL_PAINT(p)->draw_bitmap(curr->deco->Xwin(), x, y, w, h, m);
  else
    (drawing ? drawing : curr->drawing)->Add(new DrawingBitmap(x, y, w, h, m, p));
  WL_PAINT(p)->draw_bitmap_sh(this, x, y, w, h, m);
}

void init_scm_active()
{
  scm_tc16_wlactive = scm_make_smob_type("active", 0);
  scm_set_smob_mark(scm_tc16_wlactive, mark_wlactive);
  scm_set_smob_free(scm_tc16_wlactive, free_wlactive);
  scm_set_smob_print(scm_tc16_wlactive, print_wlactive);
#include "active.x"
}
