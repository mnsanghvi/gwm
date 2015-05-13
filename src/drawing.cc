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


 /***********************************\
 * 		                     *
 *    Drawing and Rubber Routines    *
 * 		                     *
 \***********************************/

#include <guile/gh.h>
#include <math.h>

#include "gwm.hh"
#include "gwmfunc.hh"
#include "error.hh"
#include "screen.hh"
#include "deco.hh"
#include "paint.hh"
#include "font.hh"
#include "active.hh"
#include "drawing.hh"

/*

struct DrawingItem {
public:
  DrawingItem() { next = 0; };
  virtual void Draw(Window win) = 0;
  DrawingItem* next;
};

struct DrawingPoint : public DrawingItem {
public:
  DrawingPoint(int xx, int yy, SCM p) { x = xx; y = yy; paint = p; scm_gc_protect_object(paint); };
  virtual ~DrawingPoint() { scm_gc_unprotect_object(paint); };
  virtual void Draw(Window win);
protected:
  int x, y;
  SCM paint;
};

struct DrawingLine : public DrawingItem {
public:
  DrawingLine(int xx1, int yy1, int xx2, int yy2, SCM p) { x1 = xx1; y1 = yy1; x2 = xx2; y2 = yy2; paint = p; scm_gc_protect_object(paint); };
  virtual ~DrawingLine() { scm_gc_unprotect_object(paint); };
  virtual void Draw(Window win);
protected:
  int x1, y1, x2, y2;
  SCM paint;
};

struct DrawingPolygon : public DrawingItem {
public:
  DrawingPolygon(int np, XPoint* pts, SCM p) { num = np; points = pts; paint = p; scm_gc_protect_object(paint); };
  virtual ~DrawingPolygon() { delete [] points; scm_gc_unprotect_object(paint); };
  virtual void Draw(Window win);
protected:
  int num;
  XPoint* points;
  SCM paint;
};

struct DrawingRect : public DrawingItem {
public:
  DrawingRect(int xx, int yy, int ww, int hh, int bb, SCM p) { x = xx; y = yy; w = ww; h = hh; bw = bb; paint = p; scm_gc_protect_object(paint); };
  virtual ~DrawingRect() { scm_gc_unprotect_object(paint); };
  virtual void Draw(Window win);
protected:
  int x, y, w, h, bw;
  SCM paint;
};

struct DrawingCircle : public DrawingItem {
public:
  DrawingCircle(int xx, int yy, int ww, int hh, int bb, int a1, int a2, SCM p) { x = xx; y = yy; w = ww; h = hh; bw = bb; ang1 = a1; ang2 = a2; paint = p; scm_gc_protect_object(paint); };
  virtual ~DrawingCircle() { scm_gc_unprotect_object(paint); };
  virtual void Draw(Window win);
protected:
  int x, y, w, h, bw;
  int ang1, ang2;
  SCM paint;
};

struct DrawingText : public DrawingItem {
public:
  DrawingText(int xx, int yy, SCM f, char* t, SCM p) { x = xx; y = yy; font = f; txt = strcpy(new char[strlen(t)+1], t); scm_gc_protect_object(font); paint = p; scm_gc_protect_object(paint); };
  virtual ~DrawingText() { delete [] txt; scm_gc_unprotect_object(font); scm_gc_unprotect_object(paint); };
  virtual void Draw(Window win);
protected:
  int x, y;
  SCM font;
  char* txt;
  SCM paint;
};

struct DrawingBitmap : public DrawingItem {
public:
  DrawingBitmap(int xx, int yy, int ww, int hh, Pixmap m, SCM p) { x = xx; y = yy; w = ww; h = hh; mask = XCreatePixmap(dpy, Context->Root(), w, h, 1); XCopyArea(dpy, m, mask, Context->gc.Shape, 0, 0, w, h, 0, 0); paint = p; scm_gc_protect_object(paint); };
  virtual ~DrawingBitmap() { XFreePixmap(dpy, mask); scm_gc_unprotect_object(paint); };
  virtual void Draw(Window win);
protected:
  int x, y, w, h;
  Pixmap mask;
  SCM paint;
};

class Drawing {
public:
  Drawing();
  ~Drawing();
  int EmptyP();
  void Draw(Window win);
  void Clear();
  void Add(DrawingItem* i);
private:
  DrawingItem* queue;
  DrawingItem* queue_tail;
};

struct RubberItem {
public:
  RubberItem(Decoration* d, long i) { deco = d; inv = i; next = 0; };
  virtual ~RubberItem() { if (next) delete next; };
  virtual void Draw() = 0;
  void DrawAll() { Draw(); if (next) next->DrawAll(); };
  void UnDrawAll() { if (next) next->UnDrawAll(); Draw(); };
  RubberItem* next;
protected:
  Decoration* deco;
  long inv;
};

struct RubberPoint : RubberItem {
public:
  RubberPoint(Decoration* d, long i, int xx, int yy) : RubberItem(d, i) { x = xx; y = yy; };
  virtual void Draw();
protected:
  int x, y;
};

struct RubberLine : RubberItem {
public:
  RubberLine(Decoration* d, long i, int xx1, int yy1, int xx2, int yy2) : RubberItem(d, i) { x1 = xx1; y1 = yy1; x2 = xx2; y2 = yy2; };
  virtual void Draw();
protected:
  int x1, y1, x2, y2;
};

struct RubberPolygon : RubberItem {
public:
  RubberPolygon(Decoration* d, long i, int np, XPoint* pts) : RubberItem(d, i) { num = np; points = pts; };
  virtual ~RubberPolygon() { delete [] points; };
  virtual void Draw();
protected:
  int num;
  XPoint* points;
};

struct RubberRect : RubberItem {
public:
  RubberRect(Decoration* d, long i, int xx, int yy, int ww, int hh, int bb) : RubberItem(d, i) { x = xx; y = yy; w = ww; h = hh; bw = bb; };
  virtual void Draw();
protected:
  int x, y, w, h, bw;
};

struct RubberCircle : RubberItem {
public:
  RubberCircle(Decoration* d, long i, int xx, int yy, int ww, int hh, int bb, int a1, int a2) : RubberItem(d, i) { x = xx; y = yy; w = ww; h = hh; bw = bb; ang1 = a1; ang2 = a2; };
  virtual void Draw();
protected:
  int x, y, w, h, bw;
  int ang1, ang2;
};

struct RubberText : RubberItem {
public:
  RubberText(Decoration* d, long i, int xx, int yy, SCM f, char* t) : RubberItem(d, i) { x = xx; y = yy; font = f; txt = strcpy(new char[strlen(t)+1], t); scm_gc_protect_object(font); };
  virtual ~RubberText() { delete [] txt; scm_gc_unprotect_object(font); };
  virtual void Draw();
protected:
  int x, y;
  SCM font;
  char* txt;
};

struct RubberBitmap : RubberItem {
public:
  RubberBitmap(Decoration* d, long i, int xx, int yy, int ww, int hh, Pixmap m) : RubberItem(d, i) { x = xx; y = yy; w = ww; h = hh; mask = XCreatePixmap(dpy, deco->Screen()->Root(), w, h, 1); XCopyArea(dpy, m, mask, deco->Screen()->gc.Shape, 0, 0, w, h, 0, 0); };
  virtual ~RubberBitmap() { XFreePixmap(dpy, mask); };
  virtual void Draw();
protected:
  int x, y, w, h;
  Pixmap mask;
};

void internal_draw_line(Drawable d, GC gc, unsigned long pixel, int x1, int y1, int x2, int y2);
void internal_draw_line_pm(Drawable d, GC gc, Pixmap pixmap, int x1, int y1, int x2, int y2);
void internal_draw_polygon(Drawable d, GC gc, unsigned long pixel, int nump, XPoint* points);
void internal_draw_polygon_pm(Drawable d, GC gc, Pixmap pixmap, int nump, XPoint* points);
void internal_draw_rect(Drawable d, GC gc, unsigned long pixel, int x, int y, int w, int h, int bord);
void internal_draw_rect_pm(Drawable d, GC gc, Pixmap pixmap, int x, int y, int w, int h, int bord);
void internal_draw_circle(Drawable d, GC gc, unsigned long pixel, int x, int y, int w, int h, int bord, int ang1, int ang2);
void internal_draw_circle_pm(Drawable d, GC gc, Pixmap pixmap, int x, int y, int w, int h, int bord, int ang1, int ang2);
void internal_draw_text(Drawable d, GC gc, unsigned long pixel, int x, int y, long fid, char* txt);
void internal_draw_text_pm(Drawable d, GC gc, Pixmap pixmap, int x, int y, long fid, char* txt);
void internal_draw_bitmap(Drawable d, GC gc, unsigned long pixel, int x, int y, int w, int h, Pixmap mask);
void internal_draw_bitmap_pm(Drawable d, GC gc, Pixmap pixmap, int x, int y, int w, int h, Pixmap mask);

int AnythingToDraw();
void DrawRubber();
void UnDrawRubber();
void ReDrawRubber();
void EraseRubber();
void ClearRubber();

*/

// The X functions XDrawArc and XFillArc seems completely weird
void XXDrawArc(Display *display, Drawable d, GC gc, int x, int y,
               unsigned int width, unsigned int height,
               int angle1, int angle2)
{
  unsigned int lw;
  XGCValues vals;
  XGetGCValues(display, gc, GCLineWidth, &vals);
  lw = vals.line_width;
  if (lw <= 0 || lw>width || lw>height)
    return;
  if (lw % 2) {
    XSetLineAttributes(display, gc, lw, LineSolid, CapButt, JoinMiter);
    XDrawArc(display, d, gc, x, y, width, height, angle1, angle2);
    XSetLineAttributes(display, gc, lw, LineSolid, CapProjecting, JoinMiter);
  } else {
    XSetLineAttributes(display, gc, lw-1, LineSolid, CapButt, JoinMiter);
    XDrawArc(display, d, gc, x, y, width-1, height-1, angle1, angle2);
    XSetLineAttributes(display, gc, 1, LineSolid, CapButt, JoinMiter);
    XDrawArc(display, d, gc, x-lw/2, y-lw/2, width+lw-1, height+lw-1, angle1, angle2);
    XSetLineAttributes(display, gc, lw, LineSolid, CapProjecting, JoinMiter);
  }
}

// Oh, no, not again! Whats the matter with the arc drawing code in X11,
// can't they get anything right ?
static int anglevec[170] =
  {1180, 1, 724, 2, 521, 3, 406, 4, 333, 5, 282, 6, 245, 7, 216, 8, 193, 9,
   175, 10, 160, 11, 147, 12, 136, 13, 127, 14, 119, 15, 112, 16, 105, 17,
   100, 18, 95, 19, 90, 20, 86, 21, 82, 22, 79, 23, 75, 24, 72, 25, 70, 26,
   67, 27, 65, 28, 63, 29, 61, 30, 59, 31, 57, 32, 55, 33, 54, 34, 52, 35,
   51, 36, 49, 37, 48, 38, 47, 39, 46, 40, 45, 41, 44, 42, 43, 43, 42, 44,
   41, 45, 40, 46, 39, 47, 38, 48, 37, 50, 36, 51, 35, 52, 34, 54, 33, 56,
   32, 57, 31, 59, 30, 61, 29, 63, 28, 65, 27, 68, 26, 71, 25, 73, 24, 76,
   23, 80, 22, 83, 21, 87, 20, 92, 19, 96, 18, 102, 17, 108, 16, 115,
   15, 122, 14, 131, 13, 141, 12, 153, 11, 167, 10, 183, 9, 204, 8, 229,
   7, 262, 6, 306, 5, 367, 4, 458, 3, 611, 2, 917, 1, 1833};

static int angletopos(int ang)
{
  int* p;
  int neg = (ang > 5760);
  if (ang >= 2880 && ang < 8640) return 0;
  if (neg) ang = 11520 - ang;
  for (p = anglevec; *p > ang; p+=2);
  return (neg ? -*(p+1) : *(p+1));
}

void XXFillArc(Display *display, Drawable d, GC gc, int x, int y,
               unsigned int width, unsigned int height,
               int angle1, int angle2)
{
  unsigned int lw;
  XGCValues vals;
  if (angle2 == 0 || width <= 0 || height <= 0)
    return;
  XGetGCValues(display, gc, GCLineWidth, &vals);
  lw = vals.line_width;
  if (width == 1 && height == 1) {
    XDrawLine(display, d, gc, x, y, x, y);
  } else if (width == 1 || height == 1) {
    XSetLineAttributes(display, gc, 1, LineSolid, CapProjecting, JoinMiter);
    if (width == 1)
      XDrawLine(display, d, gc, x, y, x, y+height-1);
    else if (height == 1)
      XDrawLine(display, d, gc, x, y, x+width-1, y);
    XSetLineAttributes(display, gc, lw, LineSolid, CapProjecting, JoinMiter);
  } else {
    XSetLineAttributes(display, gc, 1, LineSolid, CapButt, JoinMiter);
    XFillArc(display, d, gc, x, y, width-1, height-1, angle1, angle2);
    XDrawArc(display, d, gc, x, y, width-1, height-1, angle1, angle2);
    if ((angle1 > 0 && angle1 < 11520 && angle1+angle2 > 0 && angle1+angle2 < 11520) &&
        (angle1 < 2880 || angle1 >= 8640 || angle1+angle2 < 2880 || angle1+angle2 >= 8640)) {
      int p1 = angletopos(angle1) + x + width/2;
      int p2 = angletopos(angle1+angle2) + x + width/2;
      int yy = y + height/2 - 1;
      if (p1 < x) p1 = x;
      else if (p1 > x+(int)width) p1 = x + width;
      if (p2 < x) p2 = x;
      else if (p2 > x+(int)width) p2 = x + width;
      XDrawLine(display, d, gc, p1, yy, p2, yy);
    }
    XSetLineAttributes(display, gc, lw, LineSolid, CapProjecting, JoinMiter);
  }
}

void internal_draw_line(Drawable d, GC gc, unsigned long pixel, int x1, int y1, int x2, int y2)
{
  if (d) {
    XSetForeground(dpy, gc, pixel);
    XDrawLine(dpy, d, gc, x1, y1, x2, y2);
  }
}

void internal_draw_line_pm(Drawable d, GC gc, Pixmap pixmap, int x1, int y1, int x2, int y2)
{
  if (d) {
    XSetTile(dpy, gc, pixmap);
    XDrawLine(dpy, d, gc, x1, y1, x2, y2);
  }
}

void internal_draw_polygon(Drawable d, GC gc, unsigned long pixel, int nump, XPoint* points)
{
  if (d) {
    XSetForeground(dpy, gc, pixel);
    XFillPolygon(dpy, d, gc, points, nump, Complex, CoordModeOrigin);
  }
}

void internal_draw_polygon_pm(Drawable d, GC gc, Pixmap pixmap, int nump, XPoint* points)
{
  if (d) {
    XSetTile(dpy, gc, pixmap);
    XFillPolygon(dpy, d, gc, points, nump, Complex, CoordModeOrigin);
  }
}

void internal_draw_rect(Drawable d, GC gc, unsigned long pixel, int x, int y, int w, int h, int bord)
{
  if (d) {
    XSetForeground(dpy, gc, pixel);
    if (bord) {
      XSetLineAttributes(dpy, gc, bord, LineSolid, CapProjecting, JoinMiter);
      XDrawRectangle(dpy, d, gc, x-(bord+1)/2, y-(bord+1)/2, w+bord, h+bord);
      XSetLineAttributes(dpy, gc, 1, LineSolid, CapProjecting, JoinMiter);
    } else {
      XFillRectangle(dpy, d, gc, x, y, w, h);
    }
  }
}

void internal_draw_rect_pm(Drawable d, GC gc, Pixmap pixmap, int x, int y, int w, int h, int bord)
{
  if (d) {
    XSetTile(dpy, gc, pixmap);
    if (bord) {
      XSetLineAttributes(dpy, gc, bord, LineSolid, CapProjecting, JoinMiter);
      XDrawRectangle(dpy, d, gc, x-(bord+1)/2, y-(bord+1)/2, w+bord, h+bord);
      XSetLineAttributes(dpy, gc, 1, LineSolid, CapProjecting, JoinMiter);
    } else {
      XFillRectangle(dpy, d, gc, x, y, w, h);
    }
  }
}

void internal_draw_circle(Drawable d, GC gc, unsigned long pixel, int x, int y, int w, int h, int bord, int ang1, int ang2)
{
  if (d) {
    XSetForeground(dpy, gc, pixel);
    if (bord) {
      XSetLineAttributes(dpy, gc, bord, LineSolid, CapProjecting, JoinMiter);
      XXDrawArc(dpy, d, gc, x-(bord+1)/2, y-(bord+1)/2, w+bord, h+bord, ang1, ang2);
      XSetLineAttributes(dpy, gc, 1, LineSolid, CapProjecting, JoinMiter);
    } else {
      XXFillArc(dpy, d, gc, x, y, w, h, ang1, ang2);
    }
  }
}

void internal_draw_circle_pm(Drawable d, GC gc, Pixmap pixmap, int x, int y, int w, int h, int bord, int ang1, int ang2)
{
  if (d) {
    XSetTile(dpy, gc, pixmap);
    if (bord) {
      XSetLineAttributes(dpy, gc, bord, LineSolid, CapProjecting, JoinMiter);
      XXDrawArc(dpy, d, gc, x-(bord+1)/2, y-(bord+1)/2, w+bord, h+bord, ang1, ang2);
      XSetLineAttributes(dpy, gc, 1, LineSolid, CapProjecting, JoinMiter);
    } else {
      XXFillArc(dpy, d, gc, x, y, w, h, ang1, ang2);
    }
  }
}

void internal_draw_text(Drawable d, GC gc, unsigned long pixel, int x, int y, long fid, char* txt)
{
  if (d) {
    XSetForeground(dpy, gc, pixel);
    XSetFont(dpy, gc, fid);
    XDrawString(dpy, d, gc, x, y, txt, strlen(txt));
  }
}

void internal_draw_text_pm(Drawable d, GC gc, Pixmap pixmap, int x, int y, long fid, char* txt)
{
  if (d) {
    XSetTile(dpy, gc, pixmap);
    XSetFont(dpy, gc, fid);
    XDrawString(dpy, d, gc, x, y, txt, strlen(txt));
  }
}

void internal_draw_bitmap(Drawable d, GC gc, unsigned long pixel, int x, int y, int w, int h, Pixmap mask)
{
  XGCValues gcval;
  if (d) {
    gcval.foreground = pixel;
    gcval.stipple = mask;
    gcval.ts_x_origin = x;
    gcval.ts_y_origin = y;
    XChangeGC(dpy, gc, 
              GCForeground | GCTileStipXOrigin | GCTileStipYOrigin | GCStipple,
              &gcval);
    XFillRectangle(dpy, d, gc, x, y, w, h);
  }
}

void internal_draw_bitmap_pm(Drawable d, GC gc, Pixmap pixmap, int x, int y, int w, int h, Pixmap mask)
{
  XGCValues gcval;
  if (d) {
    gcval.tile = pixmap;
    gcval.clip_mask = mask;
    gcval.clip_x_origin = x;
    gcval.clip_y_origin = y;
    XChangeGC(dpy, gc, 
              GCTile | GCClipXOrigin | GCClipYOrigin | GCClipMask,
              &gcval);
    XFillRectangle(dpy, d, gc, x, y, w, h);
    XSetClipMask(dpy, gc, None);
  }
}

void DrawingPoint::Draw(Window win)
{
  WL_PAINT(paint)->draw_point(win, x, y);
}

void DrawingLine::Draw(Window win)
{
  WL_PAINT(paint)->draw_line(win, x1, y1, x2, y2);
}

void DrawingPolygon::Draw(Window win)
{
  WL_PAINT(paint)->draw_polygon(win, num, points);
}

void DrawingRect::Draw(Window win)
{
  WL_PAINT(paint)->draw_rect(win, x, y, w, h, bw);
}

void DrawingCircle::Draw(Window win)
{
  WL_PAINT(paint)->draw_circle(win, x, y, w, h, bw, ang1, ang2);
}

void DrawingText::Draw(Window win)
{
  WL_PAINT(paint)->draw_text(win, x, y, WL_FONT(font), txt);
}

void DrawingBitmap::Draw(Window win)
{
  WL_PAINT(paint)->draw_bitmap(win, x, y, w, h, mask);
}

Drawing::Drawing()
{
  queue = 0;
  queue_tail = 0;
}

Drawing::~Drawing()
{
  DrawingItem* i;
  while (queue) {
    i = queue;
    queue = queue->next;
    delete i;
  }
}

int Drawing::EmptyP()
{
  return (queue == 0);
}

void Drawing::Draw(Window win)
{
  DrawingItem* i = queue;
  while (i) {
    i->Draw(win);
    i = i->next;
  }
}

void Drawing::Clear()
{
  DrawingItem* i;
  while (queue) {
    i = queue;
    queue = queue->next;
    delete i;
  }
  queue = 0;
  queue_tail = 0;
}

void Drawing::Add(DrawingItem* i)
{
  if (queue_tail)
    queue_tail->next = i;
  else
    queue = i;
  queue_tail = i;
}

/*  Rubber drawing routines */

static RubberItem* new_drawing = 0;
static RubberItem* new_drawing_tail = 0;
static RubberItem* old_drawing = 0;
static RubberItem* old_drawing_tail = 0;
static RubberItem* temp_drawing = 0;
static RubberItem* temp_drawing_tail = 0;

void RubberPoint::Draw()
{
  internal_draw_line(deco->Xwin(), deco->Screen()->gc.Draw, inv, x, y, x, y);
}

void RubberLine::Draw()
{
  internal_draw_line(deco->Xwin(), deco->Screen()->gc.Draw, inv, x1, y1, x2, y2);
}

void RubberPolygon::Draw()
{
  internal_draw_polygon(deco->Xwin(), deco->Screen()->gc.Draw, inv, num, points);
}

void RubberRect::Draw()
{
  internal_draw_rect(deco->Xwin(), deco->Screen()->gc.Draw, inv, x, y, w, h, bw);
}

void RubberCircle::Draw()
{
  internal_draw_circle(deco->Xwin(), deco->Screen()->gc.Draw, inv, x, y, w, h, bw, ang1, ang2);
}

void RubberText::Draw()
{
  internal_draw_text(deco->Xwin(), deco->Screen()->gc.Draw, inv, x, y, WL_FONT(font)->id(), txt);
}

void RubberBitmap::Draw()
{
  internal_draw_bitmap(deco->Xwin(), deco->Screen()->gc.Draw, inv, x, y, w, h, mask);
}

int AnythingToDraw()
{
  return (new_drawing ? 1 : 0);
}

void DrawRubber()
{
  if (new_drawing) {
    new_drawing->DrawAll();
    if (old_drawing && old_drawing_tail)
      old_drawing_tail->next = new_drawing;
    else
      old_drawing = new_drawing;
    old_drawing_tail = new_drawing_tail;
    new_drawing = 0;
    new_drawing_tail = 0;
  }
}

void UnDrawRubber()
{
  if (old_drawing)
    old_drawing->UnDrawAll();
  temp_drawing_tail = old_drawing_tail;
  temp_drawing = old_drawing;
  old_drawing = 0;
  old_drawing_tail = 0;
}

void ReDrawRubber()
{
  old_drawing_tail = temp_drawing_tail;
  old_drawing = temp_drawing;
  temp_drawing = 0;
  temp_drawing_tail = 0;
  if (old_drawing)
    old_drawing->DrawAll();
}

void EraseRubber()
{
  if (old_drawing) {
    old_drawing->UnDrawAll();
    delete old_drawing;
    old_drawing = 0;
    old_drawing_tail = 0;
  }
}

void ClearRubber()
{
  if (new_drawing) {
    delete new_drawing;
    new_drawing = 0;
    new_drawing_tail = 0;
  }
}
  
void AddRubber(RubberItem* r)
{
  if (new_drawing_tail)
    new_drawing_tail->next = r;
  else
    new_drawing = r;
  new_drawing_tail = r;
}

SCM_DEFINE(draw_rubber_point, "draw-rubber-point", 3, 0, 1,
           (SCM deco, SCM x, SCM y, SCM args),
           "Draw rubber point at (x, y) on decoration. Rubber drawings are"
           "shown during 'with-user-feedback' and 'with-timer-feedback', and are"
           "erased automatically. The only possible key is:"
           ":invert-color     color to draw with XOR")
{
    SCM sub, ctx;
    int n, cn;
    SCM fg;
    Decoration* d;
    long inv;
    n = wl_separate_context(args, sub, ctx, cn, s_draw_rubber_point);
    if (n != 0)
      gwm_wrong_num_args(s_draw_rubber_point, n+3);
    must_be_valid_deco(s_draw_rubber_point, deco, 1);
    must_be_number(s_draw_rubber_point, x, 2);
    must_be_number(s_draw_rubber_point, y, 3);
    d = WL_DECO(deco);
    inv = wl_getint(gwm_get_keyword(k_invert_color, ctx, cn, gh_int2scm(WL_PAINT(d->Screen()->pixel.Black)->Pixel() ^ WL_PAINT(d->Screen()->pixel.White)->Pixel())), s_draw_rubber_point);
    AddRubber(new RubberPoint(d, inv, gh_scm2int(x), gh_scm2int(y)));
    return SCM_UNSPECIFIED;
}

SCM_DEFINE(draw_rubber_line, "draw-rubber-line", 5, 0, 1,
           (SCM deco, SCM x1, SCM y1, SCM x2, SCM y2, SCM args),
           "Draw a rubber line from (x1, y1) to (x2, y2) on decoration. Rubber drawings"
           "are shown during 'with-user-feedback' and 'with-timer-feedback', and are"
           "erased automatically. The only possible key is:"
           ":invert-color     color to draw with XOR")
{
    SCM sub, ctx;
    int n, cn;
    SCM fg;
    Decoration* d;
    long inv;
    n = wl_separate_context(args, sub, ctx, cn, s_draw_rubber_line);
    if (n != 0)
      gwm_wrong_num_args(s_draw_rubber_line, n+5);
    must_be_valid_deco(s_draw_rubber_line, deco, 1);
    must_be_number(s_draw_rubber_line, x1, 2);
    must_be_number(s_draw_rubber_line, y1, 3);
    must_be_number(s_draw_rubber_line, x2, 4);
    must_be_number(s_draw_rubber_line, y2, 5);
    d = WL_DECO(deco);
    inv = wl_getint(gwm_get_keyword(k_invert_color, ctx, cn, gh_int2scm(WL_PAINT(d->Screen()->pixel.Black)->Pixel() ^ WL_PAINT(d->Screen()->pixel.White)->Pixel())), s_draw_rubber_line);
    AddRubber(new RubberLine(d, inv, gh_scm2int(x1), gh_scm2int(y1),
                             gh_scm2int(x2), gh_scm2int(y2)));
    return SCM_UNSPECIFIED;
}

SCM_DEFINE(draw_rubber_polygon, "draw-rubber-polygon", 1, 0, 1,
           (SCM deco, SCM args),
           "Fill a rubber polygon inside the given set of points. Rubber drawings"
           "are shown during 'with-user-feedback' and 'with-timer-feedback', and are"
           "erased automatically. The only possible key is:"
           ":invert-color     color to draw with XOR")
{
    SCM sub, ctx;
    int n, cn, nump, i;
    SCM fg;
    Decoration* d;
    long inv;
    XPoint* points;
    n = wl_separate_context(args, sub, ctx, cn, s_draw_rubber_polygon);
    if (n % 2)
      gwm_wrong_num_args(s_draw_rubber_polygon, n+1);
    must_be_valid_deco(s_draw_rubber_polygon, deco, 1);
    nump = n / 2;
    points = new XPoint[nump];
    args = sub;
    for (i=0; i<nump; i++) {
      if (!scm_is_integer(SCM_CAR(args))) {
        delete [] points;
        gwm_wrong_type_arg(s_draw_rubber_polygon, i*2+2, SCM_CAR(args), "number");
      }
      points[i].x = gh_scm2int(SCM_CAR(args));
      args = SCM_CDR(args);
      if (!scm_is_integer(SCM_CAR(args))) {
        delete [] points;
        gwm_wrong_type_arg(s_draw_rubber_polygon, i*2+3, SCM_CAR(args), "number");
      }
      points[i].y = gh_scm2int(SCM_CAR(args));
      args = SCM_CDR(args);
    }
    d = WL_DECO(deco);
    inv = wl_getint(gwm_get_keyword(k_invert_color, ctx, cn, gh_int2scm(WL_PAINT(d->Screen()->pixel.Black)->Pixel() ^ WL_PAINT(d->Screen()->pixel.White)->Pixel())), s_draw_rubber_polygon);
    AddRubber(new RubberPolygon(d, inv, nump, points));
    return SCM_UNSPECIFIED;
}

SCM_DEFINE(draw_rubber_rectangle, "draw-rubber-rectangle", 5, 0, 1,
           (SCM deco, SCM x, SCM y, SCM width, SCM height, SCM args),
           "Draw a rubber rectangle on decoration, with interor starting at"
           "(x, y) and dimensions width * height. Rubber drawings are shown"
           "during 'with-user-feedback' and 'with-timer-feedback', and are"
           "erased automatically. Keys are:"
           ":borderwidth      size of frame"
           ":invert-color     color to draw with XOR")
{
    SCM sub, ctx;
    int n, cn;
    int xx, yy, ww, hh, bw;
    Decoration* d;
    long inv;
    n = wl_separate_context(args, sub, ctx, cn, s_draw_rubber_rectangle);
    if (n != 0)
      gwm_wrong_num_args(s_draw_rubber_rectangle, n+5);
    must_be_valid_deco(s_draw_rubber_rectangle, deco, 1);
    must_be_number(s_draw_rubber_rectangle, x, 2);
    must_be_number(s_draw_rubber_rectangle, y, 3);
    must_be_number(s_draw_rubber_rectangle, width, 4);
    must_be_number(s_draw_rubber_rectangle, height, 5);
    d = WL_DECO(deco);
    xx = gh_scm2int(x);
    yy = gh_scm2int(y);
    ww = gh_scm2int(width);
    hh = gh_scm2int(height);
    if (ww < 0) ww = -ww, xx -= ww;
    if (hh < 0) hh = -hh, yy -= hh;
    inv = wl_getint(gwm_get_keyword(k_invert_color, ctx, cn, gh_int2scm(WL_PAINT(d->Screen()->pixel.Black)->Pixel() ^ WL_PAINT(d->Screen()->pixel.White)->Pixel())), s_draw_rubber_rectangle);
    bw = wl_getint(gwm_get_keyword(k_borderwidth, ctx, cn, gh_int2scm(1)), s_draw_rubber_rectangle);
    AddRubber(new RubberRect(d, inv, xx, yy, ww, hh, bw));
    return SCM_UNSPECIFIED;
}

SCM_DEFINE(draw_rubber_ellipse, "draw-rubber-ellipse", 5, 0, 1,
           (SCM deco, SCM x, SCM y, SCM width, SCM height, SCM args),
           "Draw a rubber ellipse on decoration, with interor inscribed in the rectangle"
           "starting at (x, y) and having dimensions width * height. Rubber drawings"
           "are shown during 'with-user-feedback' and 'with-timer-feedback', and are"
           "erased automatically. Keys are:"
           ":borderwidth      size of frame"
           ":invert-color     color to draw with XOR")
{
    SCM sub, ctx;
    int n, cn;
    int xx, yy, ww, hh, bw;
    Decoration* d;
    long inv;
    n = wl_separate_context(args, sub, ctx, cn, s_draw_rubber_ellipse);
    if (n != 0)
      gwm_wrong_num_args(s_draw_rubber_ellipse, n+5);
    must_be_valid_deco(s_draw_rubber_ellipse, deco, 1);
    must_be_number(s_draw_rubber_ellipse, x, 2);
    must_be_number(s_draw_rubber_ellipse, y, 3);
    must_be_number(s_draw_rubber_ellipse, width, 4);
    must_be_number(s_draw_rubber_ellipse, height, 5);
    d = WL_DECO(deco);
    xx = gh_scm2int(x);
    yy = gh_scm2int(y);
    ww = gh_scm2int(width);
    hh = gh_scm2int(height);
    if (ww < 0) ww = -ww, xx -= ww;
    if (hh < 0) hh = -hh, yy -= hh;
    inv = wl_getint(gwm_get_keyword(k_invert_color, ctx, cn, gh_int2scm(WL_PAINT(d->Screen()->pixel.Black)->Pixel() ^ WL_PAINT(d->Screen()->pixel.White)->Pixel())), s_draw_rubber_ellipse);
    bw = wl_getint(gwm_get_keyword(k_borderwidth, ctx, cn, gh_int2scm(1)), s_draw_rubber_ellipse);
    AddRubber(new RubberCircle(d, inv, xx, yy, ww, hh, bw, 0, 23040));
    return SCM_UNSPECIFIED;
}

SCM_DEFINE(draw_rubber_circle_sector, "draw-rubber-circle-sector", 4, 0, 1,
           (SCM deco, SCM x, SCM y, SCM radius, SCM args),
           "Draw a rubber circle sector or arc on decoration, with center at (x, y) and"
           "the given radius of the interior, and between the angles in degrees,"
           "clockwise from the top. Rubber drawings are shown during 'with-user-feedback'"
           "and 'with-timer-feedback', and are erased automatically. Keys are:"
           ":borderwidth      size of arc"
           ":invert-color     color to draw with XOR")
{
    SCM sub, ctx;
    int n, cn;
    int bw, rad, ang1, ang2;
    Decoration* d;
    long inv;
    n = wl_separate_context(args, sub, ctx, cn, s_draw_rubber_circle_sector);
    if (n != 0 && n != 2)
      gwm_wrong_num_args(s_draw_rubber_circle_sector, n+4);
    must_be_valid_deco(s_draw_rubber_circle_sector, deco, 1);
    must_be_number(s_draw_rubber_circle_sector, x, 2);
    must_be_number(s_draw_rubber_circle_sector, y, 3);
    must_be_number(s_draw_rubber_circle_sector, radius, 4);
    rad = gh_scm2int(radius);
    if (n == 2) {
      float angle1, angle2;
      if (!gh_number_p(SCM_CAR(args)))
        gwm_wrong_type_arg(s_draw_rubber_circle_sector, 5, SCM_CAR(args), "real number");
      if (!gh_number_p(SCM_CAR(SCM_CDR(args))))
        gwm_wrong_type_arg(s_draw_rubber_circle_sector, 6, SCM_CAR(SCM_CDR(args)), "real number");
      angle1 = gh_scm2double(SCM_CAR(args));
      angle2 = gh_scm2double(SCM_CAR(SCM_CDR(args)));
      if (rad < 0) angle1 += 180.0, angle2 += 180.0, rad = -rad;
      ang1 = Round((90.0 - angle2) * 64) % 23040;
      if (ang1 < 0) ang1 += 23040;
      ang2 = (Round((90.0 - angle1) * 64) - ang1) % 23040;
      if (ang2 <= 0) ang2 += 23040;
    } else {
      ang1 = 0;
      ang2 = 23040;
      if (rad < 0) rad = -rad;
    }
    d = WL_DECO(deco);
    inv = wl_getint(gwm_get_keyword(k_invert_color, ctx, cn, gh_int2scm(WL_PAINT(d->Screen()->pixel.Black)->Pixel() ^ WL_PAINT(d->Screen()->pixel.White)->Pixel())), s_draw_rubber_circle_sector);
    bw = wl_getint(gwm_get_keyword(k_borderwidth, ctx, cn, gh_int2scm(1)), s_draw_rubber_circle_sector);
    AddRubber(new RubberCircle(d, inv, 
                               gh_scm2int(x) - rad, gh_scm2int(y) - rad,
                               rad*2, rad*2, bw, ang1, ang2));
    return SCM_UNSPECIFIED;
}

SCM_DEFINE(draw_rubber_text, "draw-rubber-text", 4, 0, 1,
           (SCM deco, SCM x, SCM y, SCM text, SCM args),
           "Draw rubber text on decoration, starting at (x, y). Rubber drawings are"
           "shown during 'with-user-feedback' and 'with-timer-feedback', and are"
           "erased automatically. Keys are:"
           ":font             font of text"
           ":angle            angle of text, clockwise in degrees"
           ":mirrored         mirror-reverse text if true"
           ":invert-color     color to draw with XOR")
{
    SCM sub, ctx;
    int n, cn;
    SCM font, ang, mir;
    Decoration* d;
    long inv;
    char* str;
    n = wl_separate_context(args, sub, ctx, cn, s_draw_rubber_text);
    if (n != 0)
      gwm_wrong_num_args(s_draw_rubber_text, n+5);
    must_be_valid_deco(s_draw_rubber_text, deco, 1);
    must_be_number(s_draw_rubber_text, x, 2);
    must_be_number(s_draw_rubber_text, y, 3);
    str = wl_getstring(text, s_draw_rubber_text, 4);
    d = WL_DECO(deco);
    inv = wl_getint(gwm_get_keyword(k_invert_color, ctx, cn, gh_int2scm(WL_PAINT(d->Screen()->pixel.Black)->Pixel() ^ WL_PAINT(d->Screen()->pixel.White)->Pixel())), s_draw_rubber_text);
    font = gwm_get_keyword(k_font, ctx, cn, DefaultFont);
    if (!WLFONTP(font))
      gwm_wrong_type_arg(s_draw_rubber_text, 0, font, "font");
    ang = gwm_get_keyword(k_angle, ctx, cn, gh_double2scm(0.0));
    if (!gh_number_p(ang))
      gwm_wrong_type_arg(s_draw_rubber_text, 0, ang, "real number");
    mir = gwm_get_keyword(k_mirrored, ctx, cn, SCM_BOOL_F);
    if (mir != SCM_BOOL_F || gh_scm2double(ang) != 0.0) {
      int xx, yy, width, height;
      Pixmap mask;
      mask = WL_FONT(font)->rotate_text(str, gh_scm2double(ang),
                                        (mir != SCM_BOOL_F ? 1 : 0),
                                        xx, yy, width, height);
      AddRubber(new RubberBitmap(d, inv, 
                                 gh_scm2int(x) - xx, gh_scm2int(y) - yy,
                                 width, height, mask));
    } else
      AddRubber(new RubberText(d, inv, gh_scm2int(x), gh_scm2int(y),
                               font, str));
    delete [] str;
    return SCM_UNSPECIFIED;
}



SCM_DEFINE(draw_point, "draw-point", 3, 0, 1,
           (SCM pixmap, SCM x, SCM y, SCM args),
           "Draw point (x, y) in pixmap or active pixmap. The only possible key is:"
           ":color")
{
    SCM sub, ctx;
    int n, cn;
    SCM fg;
    n = wl_separate_context(args, sub, ctx, cn, s_draw_point);
    if (n != 0)
      gwm_wrong_num_args(s_draw_point, n+3);
    if (!WLPIXMAPP(pixmap) && !WLACTIVEP(pixmap))
      gwm_wrong_type_arg(s_draw_point, 1, pixmap, "pixmap or active pixmap");
    must_be_number(s_draw_point, x, 2);
    must_be_number(s_draw_point, y, 3);
    fg = gwm_get_keyword(k_color, ctx, cn, Context->pixel.Black);
    if (!WLPAINTP(fg))
      gwm_wrong_type_arg(s_draw_point, 0, fg, "color or pixmap");
    if (WLPIXMAPP(pixmap))
      WL_PAINT(fg)->draw_point(WL_PIXMAP(pixmap), gh_scm2int(x), gh_scm2int(y));
    else
      WL_ACTIVE(pixmap)->add_point(gh_scm2int(x), gh_scm2int(y), fg);
    return SCM_UNSPECIFIED;
}

SCM_DEFINE(draw_line, "draw-line", 5, 0, 1,
           (SCM pixmap, SCM x1, SCM y1, SCM x2, SCM y2, SCM args),
           "Draw a line from (x1, y1) to (x2, y2) in pixmap or active pixmap."
           "The only possible key is:"
           ":color")
{
    SCM sub, ctx;
    int n, cn;
    SCM fg;
    n = wl_separate_context(args, sub, ctx, cn, s_draw_line);
    if (n != 0)
      gwm_wrong_num_args(s_draw_line, n+5);
    if (!WLPIXMAPP(pixmap) && !WLACTIVEP(pixmap))
      gwm_wrong_type_arg(s_draw_line, 1, pixmap, "pixmap or active pixmap");
    must_be_number(s_draw_line, x1, 2);
    must_be_number(s_draw_line, y1, 3);
    must_be_number(s_draw_line, x2, 4);
    must_be_number(s_draw_line, y2, 5);
    fg = gwm_get_keyword(k_color, ctx, cn, Context->pixel.Black);
    if (!WLPAINTP(fg))
      gwm_wrong_type_arg(s_draw_line, 0, fg, "color or pixmap");
    if (WLPIXMAPP(pixmap))
      WL_PAINT(fg)->draw_line(WL_PIXMAP(pixmap), gh_scm2int(x1), gh_scm2int(y1),
                              gh_scm2int(x2), gh_scm2int(y2));
    else
      WL_ACTIVE(pixmap)->add_line(gh_scm2int(x1), gh_scm2int(y1),
                                  gh_scm2int(x2), gh_scm2int(y2), fg);
    return SCM_UNSPECIFIED;
}

SCM_DEFINE(draw_polygon, "draw-polygon", 1, 0, 1,
           (SCM pixmap, SCM args),
           "Fill the polygon inside the given set of points in pixmap or active pixmap."
           "The only possible key is:"
           ":color")
{
    SCM sub, ctx;
    int n, cn, nump, i;
    SCM fg;
    XPoint* points;
    n = wl_separate_context(args, sub, ctx, cn, s_draw_polygon);
    if (n % 2)
      gwm_wrong_num_args(s_draw_polygon, n+1);
    if (!WLPIXMAPP(pixmap) && !WLACTIVEP(pixmap))
      gwm_wrong_type_arg(s_draw_polygon , 1, pixmap, "pixmap or active pixmap");
    nump = n / 2;
    points = new XPoint[nump];
    args = sub;
    for (i=0; i<nump; i++) {
      if (!scm_is_integer(SCM_CAR(args))) {
        delete [] points;
        gwm_wrong_type_arg(s_draw_polygon, i*2+2, SCM_CAR(args), "number");
      }
      points[i].x = gh_scm2int(SCM_CAR(args));
      args = SCM_CDR(args);
      if (!scm_is_integer(SCM_CAR(args))) {
        delete [] points;
        gwm_wrong_type_arg(s_draw_polygon, i*2+3, SCM_CAR(args), "number");
      }
      points[i].y = gh_scm2int(SCM_CAR(args));
      args = SCM_CDR(args);
    }
    fg = gwm_get_keyword(k_color, ctx, cn, Context->pixel.Black);
    if (!WLPAINTP(fg))
      gwm_wrong_type_arg(s_draw_polygon, 0, fg, "color or pixmap");
    if (WLPIXMAPP(pixmap))
      WL_PAINT(fg)->draw_polygon(WL_PIXMAP(pixmap), nump, points);
    else
      WL_ACTIVE(pixmap)->add_polygon(nump, points, fg);
    delete [] points;
    return SCM_UNSPECIFIED;
}

SCM_DEFINE(draw_rectangle, "draw-rectangle", 5, 0, 1,
           (SCM pixmap, SCM x, SCM y, SCM width, SCM height, SCM args),
           "Draw a rectangle in pixmap or active pixmap, with interor starting"
           "at (x, y) and dimensions width * height. Keys are:"
           ":background       color of interior"
           ":foreground       color of frame"
           ":borderwidth      size of frame")
{
    SCM sub, ctx;
    int n, cn;
    SCM fg, bg;
    int xx, yy, ww, hh, bw;
    n = wl_separate_context(args, sub, ctx, cn, s_draw_rectangle);
    if (n != 0)
      gwm_wrong_num_args(s_draw_rectangle, n+5);
    if (!WLPIXMAPP(pixmap) && !WLACTIVEP(pixmap))
      gwm_wrong_type_arg(s_draw_rectangle , 1, pixmap, "pixmap");
    must_be_number(s_draw_rectangle, x, 2);
    must_be_number(s_draw_rectangle, y, 3);
    must_be_number(s_draw_rectangle, width, 4);
    must_be_number(s_draw_rectangle, height, 5);
    bg = gwm_get_keyword(k_background, ctx, cn, SCM_UNDEFINED);
    fg = gwm_get_keyword(k_foreground, ctx, cn, SCM_UNDEFINED);
    if (fg == SCM_UNDEFINED && bg == SCM_UNDEFINED)
      fg = Context->pixel.Black;
    bw = wl_getint(gwm_get_keyword(k_borderwidth, ctx, cn, gh_int2scm(1)), s_draw_rectangle);
    xx = gh_scm2int(x);
    yy = gh_scm2int(y);
    ww = gh_scm2int(width);
    hh = gh_scm2int(height);
    if (ww < 0) ww = -ww, xx -= ww;
    if (hh < 0) hh = -hh, yy -= hh;
    if (bg != SCM_UNDEFINED) {	/* inside rectangle */
      if (!WLPAINTP(bg))
        gwm_wrong_type_arg(s_draw_rectangle, 0, bg, "color or pixmap");
      if (WLPIXMAPP(pixmap))
        WL_PAINT(bg)->draw_rect(WL_PIXMAP(pixmap), xx, yy, ww, hh, 0);
      else
        WL_ACTIVE(pixmap)->add_rect(xx, yy, ww, hh, 0, bg);
    }
    if (bw > 0 && fg != SCM_UNDEFINED) {	/* ouline rectangle */
      if (!WLPAINTP(fg))
        gwm_wrong_type_arg(s_draw_rectangle, 0, fg, "color or pixmap");
      if (WLPIXMAPP(pixmap))
        WL_PAINT(fg)->draw_rect(WL_PIXMAP(pixmap), xx, yy, ww, hh, bw);
      else
        WL_ACTIVE(pixmap)->add_rect(xx, yy, ww, hh, bw, fg);
    }
    return SCM_UNSPECIFIED;
}

SCM_DEFINE(draw_ellipse, "draw-ellipse", 5, 0, 1,
           (SCM pixmap, SCM x, SCM y, SCM width, SCM height, SCM args),
           "Draw an ellipse in pixmap or active pixmap, with interor inscribed in"
           "the rectangle starting at (x, y) and having dimensions width * height."
           "Keys are:"
           ":background       color of interior"
           ":foreground       color of frame"
           ":borderwidth      size of frame")
{
    SCM sub, ctx;
    int n, cn;
    SCM fg, bg;
    int xx, yy, ww, hh, bw;
    n = wl_separate_context(args, sub, ctx, cn, s_draw_ellipse);
    if (n != 0)
      gwm_wrong_num_args(s_draw_ellipse, n+5);
    if (!WLPIXMAPP(pixmap) && !WLACTIVEP(pixmap))
      gwm_wrong_type_arg(s_draw_ellipse , 1, pixmap, "pixmap or active pixmap");
    must_be_number(s_draw_ellipse, x, 2);
    must_be_number(s_draw_ellipse, y, 3);
    must_be_number(s_draw_ellipse, width, 4);
    must_be_number(s_draw_ellipse, height, 5);
    bg = gwm_get_keyword(k_background, ctx, cn, SCM_UNDEFINED);
    fg = gwm_get_keyword(k_foreground, ctx, cn, SCM_UNDEFINED);
    if (fg == SCM_UNDEFINED && bg == SCM_UNDEFINED)
      fg = Context->pixel.Black;
    bw = wl_getint(gwm_get_keyword(k_borderwidth, ctx, cn, gh_int2scm(1)), s_draw_ellipse);
    xx = gh_scm2int(x);
    yy = gh_scm2int(y);
    ww = gh_scm2int(width);
    hh = gh_scm2int(height);
    if (ww < 0) ww = -ww, xx -= ww;
    if (hh < 0) hh = -hh, yy -= hh;
    if (bg != SCM_UNDEFINED) {	/* inside ellipse */
      if (!WLPAINTP(bg))
        gwm_wrong_type_arg(s_draw_ellipse, 0, bg, "color or pixmap");
      if (WLPIXMAPP(pixmap))
        WL_PAINT(bg)->draw_circle(WL_PIXMAP(pixmap), xx, yy, ww, hh, 0);
      else
        WL_ACTIVE(pixmap)->add_circle(xx, yy, ww, hh, 0, 0, 23040, bg);
    }
    if (bw > 0 && fg != SCM_UNDEFINED) {	/* ouline ellipse */
      if (!WLPAINTP(fg))
        gwm_wrong_type_arg(s_draw_ellipse, 0, fg, "color or pixmap");
      if (WLPIXMAPP(pixmap))
        WL_PAINT(fg)->draw_circle(WL_PIXMAP(pixmap), xx, yy, ww, hh, bw);
      else
        WL_ACTIVE(pixmap)->add_circle(xx, yy, ww, hh, bw, 0, 23040, fg);
    }
    return SCM_UNSPECIFIED;
}

SCM_DEFINE(draw_circle_sector, "draw-circle-sector", 4, 0, 1,
           (SCM pixmap, SCM x, SCM y, SCM radius, SCM args),
           "Draw a circle sector (or an arc) in pixmap or active pixmap, with center"
           "at (x, y) and the given radius of the interior, between optional angles"
           "angle1 and angle2 in degrees counting clockwise from the top. Keys are:"
           ":background       color of the sector"
           ":foreground       color of the arc"
           ":borderwidth      width of the arc")
{
    SCM sub, ctx;
    int n, cn;
    SCM fg, bg;
    int bw, rad, ang1, ang2;
    n = wl_separate_context(args, sub, ctx, cn, s_draw_circle_sector);
    if (n != 0 && n != 2)
      gwm_wrong_num_args(s_draw_circle_sector, n+4);
    if (!WLPIXMAPP(pixmap) && !WLACTIVEP(pixmap))
      gwm_wrong_type_arg(s_draw_circle_sector, 1, pixmap, "pixmap or active pixmap");
    must_be_number(s_draw_circle_sector, x, 2);
    must_be_number(s_draw_circle_sector, y, 3);
    must_be_number(s_draw_circle_sector, radius, 4);
    rad = gh_scm2int(radius);
    if (n == 2) {
      float angle1, angle2;
      if (!gh_number_p(SCM_CAR(args)))
        gwm_wrong_type_arg(s_draw_circle_sector, 5, SCM_CAR(args), "real number");
      if (!gh_number_p(SCM_CAR(SCM_CDR(args))))
        gwm_wrong_type_arg(s_draw_circle_sector, 6, SCM_CAR(SCM_CDR(args)), "real number");
      angle1 = gh_scm2double(SCM_CAR(args));
      angle2 = gh_scm2double(SCM_CAR(SCM_CDR(args)));
      if (rad < 0) angle1 += 180.0, angle2 += 180.0, rad = -rad;
      ang1 = Round((90.0 - angle2) * 64) % 23040;
      if (ang1 < 0) ang1 += 23040;
      ang2 = (Round((90.0 - angle1) * 64) - ang1) % 23040;
      if (ang2 <= 0) ang2 += 23040;
    } else {
      ang1 = 0;
      ang2 = 23040;
      if (rad < 0) rad = -rad;
    }
    bg = gwm_get_keyword(k_background, ctx, cn, SCM_UNDEFINED);
    fg = gwm_get_keyword(k_foreground, ctx, cn, SCM_UNDEFINED);
    if (fg == SCM_UNDEFINED && bg == SCM_UNDEFINED)
      fg = Context->pixel.Black;
    bw = wl_getint(gwm_get_keyword(k_borderwidth, ctx, cn, gh_int2scm(1)), s_draw_circle_sector);
    if (bg != SCM_UNDEFINED) {	/* inside circle */
      if (!WLPAINTP(bg))
        gwm_wrong_type_arg(s_draw_circle_sector, 0, bg, "color or pixmap");
      if (WLPIXMAPP(pixmap))
        WL_PAINT(bg)->draw_circle(WL_PIXMAP(pixmap),
                                  gh_scm2int(x) - rad, gh_scm2int(y) - rad,
                                  rad*2, rad*2, 0, ang1, ang2);
      else
        WL_ACTIVE(pixmap)->add_circle(gh_scm2int(x) - rad, gh_scm2int(y) - rad,
                                      rad*2, rad*2, 0, ang1, ang2, bg);
    }
    if (bw > 0 && fg != SCM_UNDEFINED) {	/* ouline circle */
      if (!WLPAINTP(fg))
        gwm_wrong_type_arg(s_draw_circle_sector, 0, fg, "color or pixmap");
      if (WLPIXMAPP(pixmap))
        WL_PAINT(fg)->draw_circle(WL_PIXMAP(pixmap),
                                  gh_scm2int(x) - rad, gh_scm2int(y) - rad,
                                  rad*2, rad*2, bw, ang1, ang2);
      else
        WL_ACTIVE(pixmap)->add_circle(gh_scm2int(x) - rad, gh_scm2int(y) - rad,
                                      rad*2, rad*2, bw, ang1, ang2, fg);
    }
    return SCM_UNSPECIFIED;
}

SCM_DEFINE(draw_text, "draw-text", 4, 0, 1,
           (SCM pixmap, SCM x, SCM y, SCM text, SCM args),
           "Draw text in pixmap or active pixmap, starting at (x, y). Keys are:"
           ":font"
           ":color"
           ":angle             angle of text, clockwise in degrees"
           ":mirrored          mirror-reverse text if true")
{
    SCM sub, ctx;
    int n, cn;
    SCM fg, font, ang, mir;
    char* str;
    n = wl_separate_context(args, sub, ctx, cn, s_draw_text);
    if (n != 0)
      gwm_wrong_num_args(s_draw_text, n+5);
    if (!WLPIXMAPP(pixmap) && !WLACTIVEP(pixmap))
      gwm_wrong_type_arg(s_draw_text, 1, pixmap, "pixmap or active pixmap");
    must_be_number(s_draw_text, x, 2);
    must_be_number(s_draw_text, y, 3);
    str = wl_getstring(text, s_draw_text, 4);
    fg = gwm_get_keyword(k_color, ctx, cn, Context->pixel.Black);
    if (!WLPAINTP(fg))
      gwm_wrong_type_arg(s_draw_text, 0, fg, "color or pixmap");
    font = gwm_get_keyword(k_font, ctx, cn, DefaultFont);
    if (!WLFONTP(font))
      gwm_wrong_type_arg(s_draw_text, 0, font, "font");
    ang = gwm_get_keyword(k_angle, ctx, cn, gh_double2scm(0.0));
    if (!gh_number_p(ang))
      gwm_wrong_type_arg(s_draw_text, 0, ang, "real number");
    mir = gwm_get_keyword(k_mirrored, ctx, cn, SCM_BOOL_F);
    if (mir != SCM_BOOL_F || gh_scm2double(ang) != 0.0) {
      int xx, yy, width, height;
      Pixmap mask;
      mask = WL_FONT(font)->rotate_text(str, gh_scm2double(ang),
                                        (mir != SCM_BOOL_F ? 1 : 0),
                                        xx, yy, width, height);
      if (WLPIXMAPP(pixmap))
        WL_PAINT(fg)->draw_bitmap(WL_PIXMAP(pixmap),
                                  gh_scm2int(x) - xx, gh_scm2int(y) - yy,
                                  width, height, mask);
      else
        WL_ACTIVE(pixmap)->add_bitmap(gh_scm2int(x) - xx, gh_scm2int(y) - yy,
                                      width, height, mask, fg);
    } else {
      if (WLPIXMAPP(pixmap))
        WL_PAINT(fg)->draw_text(WL_PIXMAP(pixmap), gh_scm2int(x), gh_scm2int(y),
                                WL_FONT(font), str);
      else
        WL_ACTIVE(pixmap)->add_text(gh_scm2int(x), gh_scm2int(y),
                                    font, str, fg);
    }
    delete [] str;

    return SCM_UNSPECIFIED;
}


void init_scm_drawing()
{
#include "drawing.x"
}
