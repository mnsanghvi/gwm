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





