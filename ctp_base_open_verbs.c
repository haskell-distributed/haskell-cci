/*
 * Copyright (c) 2012-2013 Parallel Scientific Labs, LLC.
 * All rights reserved.
 *
 * This code is derived from software contributed to The NetBSD Foundation
 * by
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. Neither the names of the copyright holders nor the names of its
 *    contributors author may be used to endorse or promote products derived
 *    from this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
 * A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
 * OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 * SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
 * LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 * DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 * THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
 * OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

// This file is used to compile CCI statically and the verbs driver is
// always used.

#include "cci.h"
#include "plugins/base/public.h"
#include "plugins/ctp/ctp.h"

cci_plugin_ctp_t *cci_ctp = NULL;
lt_dlhandle cci_plugins_ctp_handle;

extern cci_plugin_ctp_t cci_ctp_verbs_plugin;
struct cci_plugin_handle *cci_all_plugins = NULL;

int cci_plugins_ctp_open(void) {

    cci_all_plugins = (struct cci_plugin_handle*)malloc(2*sizeof(struct cci_plugin_handle));
    cci_all_plugins->plugin = (cci_plugin_t*)&cci_ctp_verbs_plugin;
    cci_all_plugins->handle = NULL;
    cci_all_plugins->init_status = 0;

    cci_all_plugins[1].plugin = NULL;
    cci_all_plugins[1].handle = NULL;
    cci_all_plugins[1].init_status = 0;

    cci_ctp = &cci_ctp_verbs_plugin;
    return CCI_SUCCESS;
}
