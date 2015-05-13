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


/******************************\
*   			       *
*   Error handling.	       *
* 			       *
\******************************/

/*  include  */
#include <guile/gh.h>

#include 	<stdio.h>
#include	<errno.h>
#include 	"gwm.hh"
#include 	"error.hh"
#include 	"deco.hh"
#include 	"event.hh"
#include 	"fsm.hh"
#include 	"screen.hh"


int ErrorStatus;

/*  routines  */

void gwm_puts(register char *string)
{
  scm_puts(string, scm_current_output_port());
}

void gwm_warning(const char* mess, SCM arg)
{
  scm_puts("Warning: ", scm_current_output_port());
  scm_display_error_message(scm_makfrom0str(mess), 
                            (arg ? scm_listify(arg, SCM_UNDEFINED) : SCM_BOOL_F),
                            scm_current_output_port());
}

void gwm_wrong_num_args(const char* subr, int num)
{
  scm_error(scm_args_number_key,
            NULL,
            "Wrong number of arguments to ~A: ~S",
            scm_listify(scm_makfrom0str(subr), gh_int2scm(num), SCM_UNDEFINED),
            SCM_BOOL_F);
}

void gwm_wrong_type_arg(const char* subr, int pos, SCM bad_value, const char* expected)
{
  scm_error(scm_arg_type_key,
            subr,
            (pos == 0) ? "Wrong type argument instead of ~A: ~S"
            : "Wrong type argument instead of ~A in position ~S: ~S",
            (pos == 0) ? scm_listify(scm_makfrom0str(expected), bad_value, SCM_UNDEFINED)
            : scm_listify(scm_makfrom0str(expected), gh_int2scm(pos), bad_value, SCM_UNDEFINED),
            SCM_BOOL_F);
}

SCM_SYMBOL(scm_internal_error_key, "internal-error");

void gwm_internal_error(const char* subr, const char* mess)
{
  scm_error(scm_internal_error_key,
            subr,
            "Internal error: ~A",
            scm_listify(scm_makfrom0str(mess), SCM_UNDEFINED),
            SCM_BOOL_F);
}

void gwm_misc_error(const char* subr, const char* mess, SCM arg)
{
  scm_error(scm_misc_error_key, subr, mess,
            (arg ? scm_listify(arg, SCM_UNDEFINED) : SCM_BOOL_F),
            SCM_BOOL_F);
}

SCM_SYMBOL(scm_no_error_key, "no-error");

void gwm_jump_out()
{
  scm_error(scm_no_error_key, 0, 0, SCM_BOOL_F, SCM_BOOL_F);
}

/* purposeful silent error handler */
int NoXError(Display* display, XErrorEvent* error)
{
    ErrorStatus = error->error_code;
    return 0;
}

/*
 * Normal verbose error handler 
 */
int XError(Display* display, XErrorEvent* error)
{
    char            msg[MAX_TEMP_STRING_SIZE], buffer[MAX_TEMP_STRING_SIZE];
    char            number[32];

    if (error->error_code != BadWindow &&
        error->error_code != BadDrawable) {
      gwm_puts("Gwm X error: ");
      sprintf(number, "%d", error->request_code);
      XGetErrorDatabaseText(dpy, "XRequest", number, "", buffer,
                            MAX_TEMP_STRING_SIZE);
      sprintf(msg, "%s(0x%x): ", buffer, (unsigned int)error->resourceid);
      gwm_puts(msg);
      XGetErrorText(display, error->error_code, msg,
                    MAX_TEMP_STRING_SIZE);
      gwm_puts(msg);
      if (error->minor_code) {
        sprintf(msg, " (minor = %d)", error->minor_code);
        gwm_puts(msg);
      }
      scm_newline(scm_current_output_port());
    }
    ErrorStatus = error->error_code;
    return 0;
}


static int error_occurred = 0;
static int error_occurred_last = 0;

SCM gwm_handle_error(void *data, SCM tag, SCM throw_args)
{
  if (tag == scm_no_error_key)
    return SCM_UNSPECIFIED;
  scm_handle_by_message_noexit(data, tag, throw_args);
  error_occurred = 1;
  return SCM_UNSPECIFIED;
}

struct body_apply_data
{
  SCM proc;
  SCM arg1;
  SCM arg2;
};

static SCM body_apply1(void *body_data)
{
  return gh_call1(((struct body_apply_data*)body_data)->proc,
                  ((struct body_apply_data*)body_data)->arg1);
}


SCM gwm_apply1_catch(SCM proc, SCM arg)
{
  int old_error_occurred = error_occurred;
  struct body_apply_data apply_data;
  SCM res;

  apply_data.proc = proc;
  apply_data.arg1 = arg;
  error_occurred = 0;
  res = scm_internal_stack_catch(SCM_BOOL_T, body_apply1, &apply_data,
                                 gwm_handle_error, (void *)"GWM");
  error_occurred_last = error_occurred;
  error_occurred = old_error_occurred;
  return res;
}

static SCM body_apply2(void *body_data)
{
  return gh_call2(((struct body_apply_data*)body_data)->proc,
                  ((struct body_apply_data*)body_data)->arg1,
                  ((struct body_apply_data*)body_data)->arg2);
}


SCM gwm_apply2_catch(SCM proc, SCM arg1, SCM arg2)
{
  int old_error_occurred = error_occurred;
  struct body_apply_data apply_data;
  SCM res;

  apply_data.proc = proc;
  apply_data.arg1 = arg1;
  apply_data.arg2 = arg2;
  error_occurred = 0;
  res = scm_internal_stack_catch(SCM_BOOL_T, body_apply2, &apply_data,
                                 gwm_handle_error, (void *)"GWM");
  error_occurred_last = error_occurred;
  error_occurred = old_error_occurred;
  return res;
}

static SCM body_applyN(void *body_data)
{
  return gh_apply(((struct body_apply_data*)body_data)->proc,
                  ((struct body_apply_data*)body_data)->arg1);
}


SCM gwm_applyN_catch(SCM proc, SCM args)
{
  int old_error_occurred = error_occurred;
  struct body_apply_data apply_data;
  SCM res;

  apply_data.proc = proc;
  apply_data.arg1 = args;
  error_occurred = 0;
  res = scm_internal_stack_catch(SCM_BOOL_T, body_applyN, &apply_data,
                                 gwm_handle_error, (void *)"GWM");
  error_occurred_last = error_occurred;
  error_occurred = old_error_occurred;
  return res;
}

int gwm_call_with_catch(scm_t_catch_body body, void* data)
{
  int old_error_occurred = error_occurred;
  int ret;

  error_occurred = 0;
  scm_internal_stack_catch(SCM_BOOL_T, body, data, gwm_handle_error, (void *)"GWM");
  ret = error_occurred;
  error_occurred_last = error_occurred;
  error_occurred = old_error_occurred;
  return (ret ? 1 : 0);
}

struct body_cleanup_data
{
  SCM thunk;
  SCM arg;
  scm_t_catch_body clean;
  void *data;
};

static SCM body_cleanup(void *body_data)
{
  body_cleanup_data* cdata = (body_cleanup_data*)body_data;
  if (cdata->arg == SCM_UNDEFINED)
    return gh_call0(cdata->thunk);
  else
    return gh_call1(cdata->thunk, cdata->arg);
}

SCM gwm_handle_cleanup(void *data, SCM tag, SCM throw_args)
{
  body_cleanup_data* cdata = (body_cleanup_data*)data;
  cdata->clean(cdata->data);
  scm_handle_by_throw(cdata->data, tag, throw_args);
  return SCM_UNSPECIFIED;
}

SCM gwm_apply_with_cleanup(SCM thunk, SCM arg, scm_t_catch_body errbody, void* data)
{
  body_cleanup_data cdata;
  cdata.thunk = thunk;
  cdata.arg = arg;
  cdata.clean = errbody;
  cdata.data = data;
  return scm_internal_stack_catch(SCM_BOOL_T, body_cleanup, &cdata, gwm_handle_cleanup, &cdata);
}

int gwm_got_error()
{
  return error_occurred_last;
}

void init_scm_error ()
{
#include "error.x"
}

/* only for debugging purposes */
void wlo(SCM obj)
{
  scm_write(obj, scm_current_output_port());
  scm_newline(scm_current_output_port());
}

void wlung(int flg)
{
  if (flg & 1) XAllowEvents(dpy, AsyncPointer, CurrentTime);
  if (flg & 2) XAllowEvents(dpy, AsyncKeyboard, CurrentTime);
  if (flg & 4) XUngrabServer(dpy);
  XSync(dpy, 0);
}

