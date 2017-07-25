#include <stddef.h>
#include <stdlib.h>
#include <string.h>
#include <sys/socket.h>
#include <sys/un.h>

int bind_unix(int sockfd, const char* path, int len) {
    struct sockaddr_un addr;
    addr.sun_family = AF_UNIX;
    strcpy(addr.sun_path, path);
    return bind(sockfd,
                (struct sockaddr*) &addr,
                offsetof(struct sockaddr_un, sun_path) + len);
}

int connect_unix(int sockfd, const char* path, int len) {
    struct sockaddr_un addr;
    addr.sun_family = AF_UNIX;
    strcpy(addr.sun_path, path);
    return connect(sockfd,
                  (struct sockaddr*) &addr,
                  offsetof(struct sockaddr_un, sun_path) + len);
}

int recv_fd(int sockfd, int flags) {
	struct msghdr msg = {0};

	// Is this even needed? Can iov not be left as NULL?
	int uselessBuf = 0;
	struct iovec iov;
	iov.iov_base = &uselessBuf;
	iov.iov_len = sizeof(uselessBuf);
	msg.msg_iov = &iov;
	msg.msg_iovlen = 1;

	char buf[CMSG_SPACE(sizeof(int))];
	struct cmsghdr* cmsg = (struct cmsghdr*) buf;
	msg.msg_control = (void*) cmsg;
	msg.msg_controllen = CMSG_LEN(sizeof(int));

	int ret = recvmsg(sockfd, &msg, flags);
	if (ret < 0) return ret;

	struct cmsghdr* cptr = CMSG_FIRSTHDR(&msg);
	int fd = *((int*) CMSG_DATA(cptr));

	return fd;
}

int send_fd(int sockfd, int fd) {
	struct msghdr msg = {0};

	// Again, is this necessary?
	int uselessBuf = 0;
	struct iovec iov;
	iov.iov_base = &uselessBuf;
	iov.iov_len = sizeof(uselessBuf);
	msg.msg_iov = &iov;
	msg.msg_iovlen = 1;

	char buf[CMSG_SPACE(sizeof(int))];
	msg.msg_control = (void*) buf;
	msg.msg_controllen = CMSG_LEN(sizeof(int));
	struct cmsghdr* cmsg = CMSG_FIRSTHDR(&msg);
	cmsg->cmsg_level = SOL_SOCKET;
	cmsg->cmsg_type = SCM_RIGHTS;
	cmsg->cmsg_len = CMSG_LEN(sizeof(int));

	int* dPtr = (int*) CMSG_DATA(cmsg);
	*dPtr = fd;

	return sendmsg(sockfd, &msg, 0);
}
