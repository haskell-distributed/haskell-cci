#ifndef TESTLIB_H
#define TESTLIB_H

#include "cci.h"

typedef struct Proc proc_t;

extern char* test_uri[100];

void poll_event(proc_t* p);

void wait_send(proc_t* p,int cid,int sid);

void wait_recv(proc_t* p,int cid,int rid);

cci_connection_t*  wait_connection(proc_t* p,int cid);

void connect(proc_t* p,int cid,char* server_uri); 

void disconnect(proc_t* p,cci_connection_t* c); 

void send(proc_t* p,cci_connection_t* c,long sid,int l);

void read_msg(proc_t* p,char msg[100]); 

void write_msg(proc_t* p,const char* msg);

int spawn(proc_t** p,int cci_pid); 

int read_uris(proc_t* p,int n);

int write_uris(proc_t** p,int n);

void finalize(proc_t *p);

#endif // TESTLIB_H

