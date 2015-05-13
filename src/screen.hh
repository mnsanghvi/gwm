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


class ScreenContext {
public:
  ScreenContext(int screen_number);
  void InitGC();
  void Initialize();
  void Close();
  int EventHandler(XEvent* evt);
  void ReconfigureScreen(XConfigureEvent* evt);
  void SetDefaultColormap();
  void SetColormapWindow(class ClientWindow* win, Colormap cm);
  void AnnounceColormap(class ClientWindow* win, Colormap cm);
  void DenounceColormap(class ClientWindow* win);
  class ClientWindow* ColormapWindow() { return InstalledColormapCW; };
  void RegisterWindow(class ClientWindow* win);
  void UnregisterWindow(class ClientWindow* win);
    struct {
      SCM Black;	// Foreground 
      SCM White;	// Background 
    } pixel;		// screen-dependent colors 
    struct {
      GC Work;
      GC Stipple;
      GC Tile;
      GC Draw;
      GC Shape;
      GC ShapeS;
      GC ShapeT;
    } gc;	       	// the GC for the screen 
    int width, height, depth;	// dims in pixels 
    int widthMM, heightMM;		// dims in millimeters 
    Pixmap DefaultBitmap;
    int ScreenNum() { return screen; };
    class Decoration* Deco() { return rootdeco; };
    Window Root() { return root; };
    ClientWindow* First() { return first; };
    Window GwmWindow() { return gwm_window; };
protected:
    void CreateGwmLabelWindow();
    int screen;		// screen number 
    Screen *x_screen;		// X structure 
    Window root;		// root window 
    class Decoration* rootdeco;		// and its associated wob 
    int WindowCount;	// number of managed windows 
    class ClientWindow* first;
    class ClientWindow* last;
    Window gwm_window;	       // window used to set properties on 
    class ClientWindow* InstalledColormapCW;	// window of inst. colormap 
    Colormap colormap;
    Colormap InstalledColormap;
};

void SetDefaultScreen();
int ScreenOfRoot(Window window);
void SetContext(ScreenContext* scr);


















