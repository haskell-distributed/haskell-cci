//
// Copyright (C) 2012 Parallel Scientific. All rights reserved.
//
// See the accompanying COPYING file for license information.
//

// This file is used to compile CCI statically and the verbs
// driver is always used.

#include "cci.h"
#include "plugins/base/public.h"
#include "plugins/ctp/ctp.h"

cci_plugin_ctp_t *cci_ctp = NULL;
lt_dlhandle cci_plugins_ctp_handle;

extern cci_plugin_ctp_t cci_ctp_sock_plugin;
struct cci_plugin_handle *cci_all_plugins = NULL;

int cci_plugins_ctp_open(void) {

    cci_all_plugins = (struct cci_plugin_handle*)malloc(2*sizeof(struct cci_plugin_handle));
    cci_all_plugins->plugin = (cci_plugin_t*)&cci_ctp_sock_plugin;
    cci_all_plugins->handle = NULL;
    cci_all_plugins->init_status = 0;

    cci_all_plugins[1].plugin = NULL;
    cci_all_plugins[1].handle = NULL;
    cci_all_plugins[1].init_status = 0;

 
    return CCI_SUCCESS;
}
