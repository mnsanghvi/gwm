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


#define TrapXErrors(statement) (\
  	XSync(dpy, 0), XSetErrorHandler(NoXError), ErrorStatus = 0,	\
	statement,  \
	XSync(dpy, 0), XSetErrorHandler(XError), ErrorStatus)

#define FlushXErrors() \
    	XSetErrorHandler(NoXError), XSync(dpy, 0), XSetErrorHandler(XError)

#define OK           0
#define FatalError  -1

extern int ErrorStatus;

void gwm_puts(register char *string);
void gwm_warning(const char* mess, SCM arg);

void gwm_wrong_num_args(const char* subr, int num);
void gwm_wrong_type_arg(const char* subr, int pos, SCM bad_value, const char* expected);
void gwm_internal_error(const char* subr, const char* mess);
void gwm_misc_error(const char* subr, const char* mess, SCM arg);
void gwm_jump_out();

extern "C" int NoXError(Display* display, XErrorEvent* error);
extern "C" int XError(Display* display, XErrorEvent* error);

SCM gwm_apply1_catch(SCM proc, SCM arg);
SCM gwm_apply2_catch(SCM proc, SCM arg1, SCM arg2);
SCM gwm_applyN_catch(SCM proc, SCM args);
int gwm_call_with_catch(scm_t_catch_body body, void* data);
SCM gwm_apply_with_cleanup(SCM thunk, SCM arg, scm_t_catch_body errbody, void* data);
int gwm_got_error();

