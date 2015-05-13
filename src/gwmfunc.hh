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


SCM gwm_get_keyword(SCM key, SCM l, int len, SCM default_value);
int wl_separate_context(SCM args, SCM& subret, SCM& ctxret, int& cn, const char *subr);
int wl_getint(SCM obj, const char* subr);
int wl_getposint(SCM obj, const char* subr, int n=0);
int wl_getbool(SCM obj, const char* subr);
char* wl_getstring(SCM obj, const char* subr, int n=0);
char* wl_getsymbol(SCM obj, const char* subr, int n=0);
void must_be_number(const char* subr, SCM obj, int n);
void must_be_string(const char* subr, SCM obj, int n);
void must_be_deco(const char* subr, SCM obj, int n);
void must_be_valid_deco(const char* subr, SCM obj, int n);
void must_be_window(const char* subr, SCM obj, int n);
void must_be_valid_window(const char* subr, SCM obj, int n);
SCM SemiDecoratedWindow(Window w);
void ConfigureUnmappedWindow(XConfigureRequestEvent* evt);
KeySym keycode_and_modifier_to_keysym(int keycode, int modifier);
int keysym_to_keycode_modifier_mask(KeySym keysym, int keycode);
void process_masked_events(int mask);
