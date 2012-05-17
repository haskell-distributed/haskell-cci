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
};

char* test_uri[100];


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
           *((void**)&(event->accept.connection->context)) = event->accept.context;
           p->conns[(int64_t)event->accept.context] = event->accept.connection;
           break;
       case CCI_EVENT_RECV:
           check_msg((const char*)event->recv.ptr,event->recv.len);
           get_msg_id((const char*)event->recv.ptr,event->recv.len,msg_id);
           fprintf(stderr,"process %d: CCI_EVENT_RECV (conn=%d,msg_id=%s)\n",pid,event->recv.connection->context,msg_id);
           sglib_int_list_t_add(&p->recvs[(int64_t)event->recv.connection->context],mk_int_list_t_node(atoi(msg_id)));
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
    cci_opt_handle_t handle;
    handle.endpoint = ep;
    char* name;
	int ret = cci_get_opt(&handle, CCI_OPT_LEVEL_ENDPOINT, CCI_OPT_ENDPT_URI, (void *)&name);
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
    memcpy(p->rbuf,null+1,p->rcount);
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

