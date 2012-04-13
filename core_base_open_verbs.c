//
// Copyright (C) 2012 Parallel Scientific. All rights reserved.
//
// See the accompanying COPYING file for license information.
//

// This file is used to compile CCI statically and the verbs
// driver is always used.

#include "cci.h"
#include "plugins/base/public.h"
#include "plugins/core/core.h"

cci_plugin_core_t *cci_core = NULL;
lt_dlhandle cci_plugins_core_handle;

extern cci_plugin_core_t cci_core_verbs_plugin;
struct cci_plugin_handle *cci_all_plugins = NULL;

int cci_plugins_core_open(void) {

    cci_all_plugins = (struct cci_plugin_handle*)malloc(2*sizeof(struct cci_plugin_handle));
    cci_all_plugins->plugin = (cci_plugin_t*)&cci_core_verbs_plugin;
    cci_all_plugins->handle = NULL;
    cci_all_plugins->init_status = 0;

    cci_all_plugins[1].plugin = NULL;
    cci_all_plugins[1].handle = NULL;
    cci_all_plugins[1].init_status = 0;

    cci_core = &cci_core_verbs_plugin;
    return CCI_SUCCESS;
}
