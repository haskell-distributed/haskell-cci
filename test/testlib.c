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

#include "testlib.h"

struct Proc {
    int fd[2];
    cci_endpoint_t* endpoint;
    char uri[100];
    int cci_pid;
    char rbuf[100];
    size_t rcount;
	cci_connection_t* conns[100];
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

void handle_event(proc_t* p,cci_event_t* event) {
	int pid = p->cci_pid;
    switch(event->type) {
       case CCI_EVENT_CONNECT_REQUEST:
           fprintf(stderr,"process %d: CCI_EVENT_CONNECT_REQUEST\n",pid);
		   cci_accept(event,((void**)event->request.data_ptr)[0]);
           break;
       case CCI_EVENT_CONNECT:
           fprintf(stderr,"process %d: CCI_EVENT_CONNECT\n",pid);
           p->conns[(int64_t)event->connect.context] = event->connect.connection;
           break;
       case CCI_EVENT_ACCEPT:
           fprintf(stderr,"process %d: CCI_EVENT_ACCEPT\n",pid);
           p->conns[(int64_t)event->connect.context] = event->accept.connection;
           break;
       case CCI_EVENT_RECV:
           fprintf(stderr,"process %d: CCI_EVENT_RECV\n",pid);
           break;
       case CCI_EVENT_SEND:
           fprintf(stderr,"process %d: CCI_EVENT_SEND\n",pid);
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



void create_endpoint(cci_endpoint_t** endpoint) {

	cci_os_handle_t ep_fd;
	int ret = cci_create_endpoint(NULL, 0, endpoint, &ep_fd);
	if (ret) {
		fprintf(stderr, "cci_create_endpoint() failed with %s\n",
			cci_strerror(*endpoint,ret));
		exit(EXIT_FAILURE);
	}
	printf("Opened %s\n", (*endpoint)->name);

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

void send(proc_t* p,cci_connection_t* c,long sid,char* csid) {
    char buf[100];
    read_msg(p,buf);

    int ret = cci_send(c,csid,strlen(csid)+1,(void*)sid,0);
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
        write_msg(*p,(*p)->endpoint->name);

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


