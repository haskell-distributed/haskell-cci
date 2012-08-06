//
// Copyright (C) 2012 Parallel Scientific. All rights reserved.
//
// See the accompanying COPYING file for license information.
//
// This file provides an implementantion of auxiliary routines 
// necessary for reproducing failing CCI test cases in C.
//

#include <stdio.h>
#include <string.h>
#include <strings.h>
#include <stdlib.h>
#include <unistd.h>
#include <inttypes.h>
#include <assert.h>
#include <sys/time.h>
#include <time.h>

#include "cci.h"
#include "sglib.h"

#include "testlib.h"

typedef struct _int_list {
    int v;
    struct _int_list* next;
} int_list_t;

int_list_t* mk_int_list_t_node(int v) {
    int_list_t* n=malloc(sizeof(int_list_t));
    n->v = v;
    n->next = NULL;
    return n;
}

void free_int_list_t_node(int_list_t* n) { free(n); }

#define INT_LIST_COMPARATOR(e1, e2) (e1->v - e2->v)

SGLIB_DEFINE_LIST_PROTOTYPES(int_list_t, INT_LIST_COMPARATOR, next);
SGLIB_DEFINE_LIST_FUNCTIONS(int_list_t, INT_LIST_COMPARATOR, next);

typedef struct _int_lh_list {
    int k;
    uint64_t lh;
    void* buf;
    int len;
    struct _int_lh_list* next;
} int_lh_list_t;

int_lh_list_t* mk_int_lh_list_t_node(int k,uint64_t lh,void* buf,int len) {
    int_lh_list_t* n=malloc(sizeof(int_lh_list_t));
    n->k = k;
    n->lh = lh;
    n->buf = buf;
    n->len = len;
    n->next = NULL;
    return n;
}

void free_int_lh_list_t_node(int_lh_list_t* n) { free(n); }

#define INT_LH_LIST_COMPARATOR(e1, e2) (e1->k - e2->k)

SGLIB_DEFINE_LIST_PROTOTYPES(int_lh_list_t, INT_LH_LIST_COMPARATOR, next);
SGLIB_DEFINE_LIST_FUNCTIONS(int_lh_list_t, INT_LH_LIST_COMPARATOR, next);

typedef struct _int_rh_list {
    int k;
    uint64_t rh;
    struct _int_rh_list* next;
} int_rh_list_t;

int_rh_list_t* mk_int_rh_list_t_node(int k,uint64_t rh) {
    int_rh_list_t* n=malloc(sizeof(int_rh_list_t));
    n->k = k;
    n->rh = rh;
    n->next = NULL;
    return n;
}

void free_int_rh_list_t_node(int_rh_list_t* n) { free(n); }

#define INT_RH_LIST_COMPARATOR(e1, e2) (e1->k - e2->k)

SGLIB_DEFINE_LIST_PROTOTYPES(int_rh_list_t, INT_RH_LIST_COMPARATOR, next);
SGLIB_DEFINE_LIST_FUNCTIONS(int_rh_list_t, INT_RH_LIST_COMPARATOR, next);



struct Proc {
    int fd[2];
    cci_endpoint_t* endpoint;
    char uri[100];
    int cci_pid;
    char rbuf[100];
    size_t rcount;
	cci_connection_t* conns[100];
    int_list_t* sends[100];
    int_list_t* recvs[100];
    // RMA state
    int_list_t* reuse;
    int_lh_list_t* reservedLocalHandles;
    int_lh_list_t* availableHandles;
    int_rh_list_t* remoteHandles;
    int_list_t* rmaWriteIds;
    int_list_t* rmaReadIds[100];
};

char* test_uri[100];
typedef enum { MSG_RMA_HANDLE, MSG_RMA_READ, MSG_OTHER } MessageType;


void check_return(char *func, cci_endpoint_t* ep, int ret, int need_exit) {
	if (ret) {
		fprintf(stderr, "%s() returned %s\n", func, cci_strerror(ep,ret));
		if (need_exit)
			exit(EXIT_FAILURE);
	}
	return;
}

void check_msg(const char* ptr,uint32_t len) {
    // Read a numeric identifier
    int pos = 0;
    while (pos<len && '0'<=ptr[pos] && ptr[pos]<='9')
        pos+=1;

    int ret=0;
    if (pos==0 || pos<len && ptr[pos]!=' ') {
        ret = 1;
        goto failed;
    }

    // Read the message body
    int i=pos+1;
    while (i<len) {
        int j=0;
        while(j<pos && i<len && ptr[j]==ptr[i]) {
            i+=1;
            j+=1;
        }
        if (j<pos && i<len) {
            ret = 2;
            goto failed;
        }
    }

    return;

  failed:

	fprintf(stderr, "check_msg() failed ");
    if (ret==1)
	    fprintf(stderr, "when reading identifier (pos: %d, len: %d).\n",pos,len);
    else
	    fprintf(stderr, "when reading message body (pos: %d, ptr[pos]: %d, %c).\n",i,ptr[i],ptr[i]);
	exit(EXIT_FAILURE);
}

void get_msg_id(const char* ptr,uint32_t len,char* res) {
    // Read a numeric identifier
    int pos = 0;
    while (pos<len && '0'<=ptr[pos] && ptr[pos]<='9') {
        res[pos]=ptr[pos];
        pos+=1;
    }
    res[pos]='\0';
}

uint64_t get_msg_remote_handle(const char* ptr) {
    uint64_t rh = 0;
    int i;
    for(i=0;i<8;i+=1)
        rh = (rh<<8) + (uint64_t)ptr[5+i];
    return rh;
}

MessageType get_message_type(const char* ptr,uint32_t len) {
    if (len>=5 && strncmp(ptr,"rmaH ",5)==0) {
        if (len!=13) {
	        fprintf(stderr, "get_message_type() failed: rmaH message has not length 13: %d\n",len);
            exit(EXIT_FAILURE);
        } else
            return MSG_RMA_HANDLE;
    } else if (len>=8 && strncmp(ptr,"rmaRead ",8)==0) {
        int i=8;
        while(i<len && '0'<=ptr[i] && ptr[i]<='9')
            i+=1;
        if (i<len) {
            char* buf = (char*)malloc(len+1);
            memcpy(buf,ptr,len);
            buf[len]='\0';
	        fprintf(stderr, "get_message_type() failed: rmaRead message is malformed: \"%s\"\n",buf);
            free(buf);
            exit(EXIT_FAILURE);
        } else
            return MSG_RMA_READ;
    } else {
        check_msg(ptr, len);
        return MSG_OTHER;
    }
}

void handle_event(proc_t* p,cci_event_t* event) {
	int pid = p->cci_pid;
    char msg_id[100];
    switch(event->type) {
       case CCI_EVENT_CONNECT_REQUEST:
           fprintf(stderr,"process %d: CCI_EVENT_CONNECT_REQUEST (conn=%d)\n",pid,((void**)event->request.data_ptr)[0]);
		   cci_accept(event,((void**)event->request.data_ptr)[0]);
           break;
       case CCI_EVENT_CONNECT:
           fprintf(stderr,"process %d: CCI_EVENT_CONNECT (conn=%d,status=%s)\n"
                   ,pid,event->connect.connection->context,cci_strerror(p->endpoint,event->connect.status));
           p->conns[(int64_t)event->connect.context] = event->connect.connection;
           break;
       case CCI_EVENT_ACCEPT:
           fprintf(stderr,"process %d: CCI_EVENT_ACCEPT (conn=%d,status=%s)\n"
                   ,pid,event->accept.context,cci_strerror(p->endpoint,event->accept.status));
           p->conns[(int64_t)event->accept.context] = event->accept.connection;
           break;
       case CCI_EVENT_RECV:
           switch(get_message_type((const char*)event->recv.ptr,event->recv.len)) {
            case MSG_RMA_HANDLE:
                sglib_int_rh_list_t_add(&p->remoteHandles,
                    mk_int_rh_list_t_node((int64_t)event->recv.connection->context
                        ,get_msg_remote_handle((const char*)event->recv.ptr)));
                break;
            case MSG_OTHER:
                get_msg_id((const char*)event->recv.ptr,event->recv.len,msg_id);
                fprintf(stderr,"process %d: CCI_EVENT_RECV (conn=%d,msg_id=%s)\n",
                        pid,event->recv.connection->context,msg_id);
                sglib_int_list_t_add(&p->recvs[(int64_t)event->recv.connection->context],
                                     mk_int_list_t_node(atoi(msg_id)));
                break;
           }
           break;
       case CCI_EVENT_SEND:
           fprintf(stderr,"process %d: CCI_EVENT_SEND (conn=%d,msg_id=%d,status=%s)\n"
                         ,pid,event->send.connection->context,(int64_t)event->send.context
                         ,cci_strerror(p->endpoint,event->send.status));
           sglib_int_list_t_add(&p->sends[(int64_t)event->send.connection->context],mk_int_list_t_node((int64_t)event->send.context));
           break;
       default:
           fprintf(stderr,"process %d: aborting with received event: %d\n",pid,event->type);
           exit(1);
           break;
    }
}

void rma_reuse(proc_t* p,int cid) {
    sglib_int_list_t_add(&p->reuse,mk_int_list_t_node(cid));
}

void rma_handle_exchange(proc_t* p,int cid) {
    // Create a local handle
    uint64_t h;
    int ret;
    int_list_t e = { cid, NULL };
    int_list_t* d;
    // if we must reuse a previous handle
    if (p->availableHandles &&
            sglib_int_list_t_delete_if_member(&p->reuse,&e,&d)) {
        free(d);

        int_lh_list_t* lh = p->availableHandles;
        sglib_int_lh_list_t_delete(&p->availableHandles,lh);

        sglib_int_lh_list_t_add(&p->reservedLocalHandles,lh);
        h = lh->lh;

    } else { // if we must create a new handle
        void* rma_buf;
        int len = 4096;
        posix_memalign(rma_buf,4096,len);
        ret=cci_rma_register(p->endpoint,rma_buf,len,CCI_FLAG_READ | CCI_FLAG_WRITE,&h);
	    check_return("cci_rma_register", p->endpoint, ret, 1);

        int_lh_list_t* lh = mk_int_lh_list_t_node(cid,h,rma_buf,len);
        sglib_int_lh_list_t_add(&p->reservedLocalHandles,lh);
    }

    // Send the handle
    const int buf_len=13;
    char buf[buf_len];
    strcpy(buf,"rmaH ");
    int i;
    for(i=0;i<8;i+=1)
        buf[5+i] = (h>>((7-i)*8)) & 0xFF;
    
    ret = cci_send(p->conns[cid],buf,buf_len,NULL,0);
	check_return("cci_send", p->endpoint, ret, 1);

    write_msg(p,"");
}

void wait_rma_exchange(proc_t* p,int cid) {
    cci_event_t *event;
    int_rh_list_t e = { cid, 0, NULL };
    while(!sglib_int_rh_list_t_is_member(p->remoteHandles,&e)) {
	    while(CCI_SUCCESS != cci_get_event(p->endpoint, &event)) {
		}
		handle_event(p,event);
		cci_return_event(event);
    }
        
    write_msg(p,"");
}

void prepare_rma_read(proc_t* p,int cid,int sid) {
    char sids[100];
    int sidlen = sprintf(sids,"%d",sid);

    int_lh_list_t k = {cid, 0, NULL, 0, NULL};
    int_lh_list_t* lh = sglib_int_lh_list_t_find_member(p->reservedLocalHandles,&k);

    int i=0;
    while(i<lh->len) {
        strncpy(lh->buf+i,sids,lh->len-i);
        i+=sidlen;
    }

    write_msg(p,"");
}

void rma_read(proc_t* p,int cid,int sid) {
    
    int_lh_list_t k = {cid, 0, NULL, 0, NULL};
    int_lh_list_t* lh = sglib_int_lh_list_t_find_member(p->reservedLocalHandles,&k);

    int_rh_list_t kr = {cid, 0, NULL};
    int_rh_list_t* rh = sglib_int_rh_list_t_find_member(p->remoteHandles,&kr);

    sglib_int_list_t_add(&p->rmaReadIds[cid],mk_int_list_t_node(sid));

    char buf[100];
    int len = sprintf(buf,"rmaRead %d",sid);
    cci_rma(p->conns[cid],buf,len,lh->lh,0,rh->rh,0,lh->len,(void*)(intptr_t)sid,CCI_FLAG_READ);

    write_msg(p,"");
}

void poll_event(proc_t* p)
{
    cci_event_t *event;
    int pid = p->cci_pid;
    char buf[100];
    read_msg(p,buf);

    while(CCI_SUCCESS != cci_get_event(p->endpoint, &event)) {
    }

	handle_event(p,event);
    cci_return_event(event);
    write_msg(p,"");
}

void wait_send(proc_t* p,int cid,int sid)
{
    cci_event_t *event;
    int pid = p->cci_pid;
    char buf[100];
    read_msg(p,buf);
    int_list_t e = { sid, NULL };
    int_list_t* d;
    
    sglib_int_list_t_delete_if_member(&p->sends[cid],&e,&d);
	while(!d) {
	    while(CCI_SUCCESS != cci_get_event(p->endpoint, &event)) {
		}
		handle_event(p,event);
		cci_return_event(event);
        sglib_int_list_t_delete_if_member(&p->sends[cid],&e,&d);
    }
    free_int_list_t_node(d);

    write_msg(p,"");
}

void wait_recv(proc_t* p,int cid,int rid)
{
    cci_event_t *event;
    int pid = p->cci_pid;
    char buf[100];
    read_msg(p,buf);
    int_list_t e = { rid, NULL };
    int_list_t* d;

    sglib_int_list_t_delete_if_member(&p->recvs[cid],&e,&d);
	while(!d) {
	    while(CCI_SUCCESS != cci_get_event(p->endpoint, &event)) {
		}
		handle_event(p,event);
		cci_return_event(event);
        sglib_int_list_t_delete_if_member(&p->recvs[cid],&e,&d);
    }
    free_int_list_t_node(d);

    write_msg(p,"");
}

cci_connection_t*  wait_connection(proc_t* p,int cid)
{
    cci_event_t *event;
    cci_connection_t* conn = NULL;
    int pid = p->cci_pid;
    char buf[100];
    read_msg(p,buf);

    // struct timespec ts = { 0, 100*1000 };
	while(!p->conns[cid]) {
	    while(CCI_SUCCESS != cci_get_event(p->endpoint, &event)) {
		   // if (pid==1) 
			 //   nanosleep(&ts,NULL);
		}
		handle_event(p,event);
		cci_return_event(event);
    }
	conn = p->conns[cid];
	p->conns[cid] = NULL;

    write_msg(p,"");
    return conn;
}


char* get_endpoint_name(cci_endpoint_t* ep) {
    char* name;
	int ret = cci_get_opt(ep, CCI_OPT_ENDPT_URI, (void *)&name);
    check_return("cci_get_opt",ep,ret,1);
    return name;
}

void create_endpoint(cci_endpoint_t** endpoint) {

	cci_os_handle_t ep_fd;
	int ret = cci_create_endpoint(NULL, 0, endpoint, &ep_fd);
	if (ret) {
		fprintf(stderr, "cci_create_endpoint() failed with %s\n",
			cci_strerror(*endpoint,ret));
		exit(EXIT_FAILURE);
	}
	printf("Opened %s\n", get_endpoint_name(*endpoint));

}

void init() {
	uint32_t caps = 0;
	int ret = cci_init(CCI_ABI_VERSION, 0, &caps);
	check_return("cci_init", NULL, ret, 1);
}

void connect(proc_t* p,int cid,char* server_uri) {
    char buf[100];
    read_msg(p,buf);
	void* data[] = { (void*)(int64_t)cid };

	int ret = cci_connect(p->endpoint,server_uri, data, sizeof(void*),CCI_CONN_ATTR_RO , (void*)(int64_t) cid, 0, NULL);
	check_return("cci_connect", p->endpoint, ret, 1);

	printf("Connecting to %s\n", server_uri);
    write_msg(p,"");
}

void disconnect(proc_t* p,cci_connection_t* c) {
    char buf[100];
    read_msg(p,buf);

    cci_disconnect(c);

    write_msg(p,"");
}

void send(proc_t* p,cci_connection_t* c,long sid,int len) {
    char buf[100];
    read_msg(p,buf);

    int n=sprintf(buf,"%ld",sid);
    char* csid = (char*)malloc(len);
    if (n<len)
        csid[n] = ' ';
    strncpy(csid,buf,n);
    int i=1+n;
    while(i<len) {
        strncpy(&csid[i],buf,n+i<=len?n:len-i);
        i+=n;
    }

    int ret = cci_send(c,csid,len,(void*)sid,0);
    free(csid);
	check_return("cci_send", c->endpoint, ret, 1);

    write_msg(p,"");
}

void read_msg(proc_t* p,char msg[100]) {
    int nnew = 0;
    char* null = memchr(p->rbuf,'\0',p->rcount);

    while (!null) {
        nnew = read(p->fd[0],p->rbuf+p->rcount,100-p->rcount);
        null = memchr(p->rbuf+p->rcount,'\0',nnew);
        p->rcount += nnew;
    }

    strcpy(msg,p->rbuf);
    p->rcount -= null+1 - p->rbuf;
    memmove(p->rbuf,null+1,p->rcount);
}

void write_msg(proc_t* p,const char* msg) {
    size_t n = write(p->fd[1],msg,strlen(msg)+1);
}


int spawn(proc_t** p,int cci_pid) {
    int fdi[2];
    int fdo[2];
    *p = (proc_t*)malloc(sizeof(proc_t));
    (*p)->endpoint = NULL;
    (*p)->uri[0] = '\0';
    (*p)->cci_pid = cci_pid;
    (*p)->rcount = 0;
	memset((*p)->conns,0,100*sizeof(cci_connection_t*));
	memset((*p)->sends,0,100*sizeof(int_list_t*));
	memset((*p)->recvs,0,100*sizeof(int_list_t*));
    (*p)->reuse = NULL;
    (*p)->availableHandles = NULL;
    (*p)->reservedLocalHandles = NULL;
    (*p)->remoteHandles = NULL;
    (*p)->rmaWriteIds = NULL;

    if (pipe(fdi)) {
        perror("Error creating pipe");
        exit(1);
    }
    if (pipe(fdo)) {
        perror("Error creating pipe");
        exit(1);
    }
    int pid = fork();
    if (pid==-1) {
		perror("Error spawning process");
        exit(1);
    } else if (pid==0) {
        close(fdi[1]);
        close(fdo[0]);

        (*p)->fd[0] = fdi[0];
        (*p)->fd[1] = fdo[1];

        init();
        create_endpoint(&(*p)->endpoint);

        write_msg(*p,get_endpoint_name((*p)->endpoint));

    } else {
        close(fdi[0]);
        close(fdo[1]);

        (*p)->fd[0] = fdo[0];
        (*p)->fd[1] = fdi[1];
        read_msg(*p,(*p)->uri);
    }

    return pid;
    
}

int read_uris(proc_t* p,int n) {
    char buf[100];
    int i;

    for(i=0;i<n;i++) {
        int k;
        char* uri = (char*)malloc(100);
        read_msg(p,buf);
        sscanf(buf,"%d %s",&k,uri);
        test_uri[k] = uri;
    }
}

int write_uris(proc_t** p,int n) {
    int i;
    int j;
    char buf[100];

    for(i=0;i<n;i++) {
        if (p[i]) {
            for(j=0;j<n;j++) {
                if (p[j]) {
                    sprintf(buf,"%d %s",j,p[j]->uri);
                    write_msg(p[i],buf);
                }
            }
        }
    }
}

void finalize(proc_t *p) {
	cci_destroy_endpoint(p->endpoint);
	cci_finalize();
	return;
}

