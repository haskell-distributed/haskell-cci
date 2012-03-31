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

extern cci_plugin_core_t cci_core_sock_plugin;

int cci_plugins_core_open(void) {
    cci_core = &cci_core_sock_plugin;
    return CCI_SUCCESS;
}
