#ifndef RF_SERVER_H
#define RF_SERVER_H
#ifdef _WIN32
  #include <winsock2.h>
  #include <ws2tcpip.h>
  typedef SOCKET rf_socket_t;
  #define RF_INVALID_SOCKET INVALID_SOCKET
  #define RF_CLOSESOCK closesocket
  #define RF_IOCTL ioctlsocket
  #define RF_LAST_SOCK_ERR() WSAGetLastError()
  #define RF_EWOULDBLOCK WSAEWOULDBLOCK
#else
  #include <sys/types.h>
  #include <sys/time.h>
  #include <sys/select.h>
  #include <unistd.h>
  #include <arpa/inet.h>
  #include <netinet/in.h>
  #include <sys/socket.h>
  #include <sys/ioctl.h>
  typedef int rf_socket_t;
  #define RF_INVALID_SOCKET (-1)
  #define RF_CLOSESOCK close
  #define RF_IOCTL ioctl
  #define RF_LAST_SOCK_ERR() errno
  #define RF_EWOULDBLOCK EWOULDBLOCK
#endif
#include <errno.h>
#define SG_TCP_BACKLOG 32
#define SG_TCP_CTRL_NAK   0x00  
#define SG_TCP_CTRL_ACK   0x01  
#define SG_TCP_CTRL_QRY   0x02  
#define SG_TCP_CTRL_NSZ   0x03  
#define SG_TCP_CTRL_TSZ   0x04  
#define SG_TCP_CTRL_EOF   0x05  
#define SG_TCP_CTRL_EOD   0x06  
#define SG_TCP_CTRL_STP   0x07  
#define SG_TCP_CTRL_TVL   0x08  
#define SG_SOCK_STATE_OPN  0x11
#define SG_SOCK_STATE_LEN  0x12
#define SG_SOCK_STATE_DTA  0x13
void server(uint port, time_t userTimeout, uint xSize, uint pSize, DescriptorObj *headDO);
int serverSend(rf_socket_t clientSockFD, char *record, int length);
int flushReadBuffer(rf_socket_t clientSockFD, DescriptorObj *currDO, char *closeConn);
extern void predictForestRT(DescriptorObj *headDO);
#endif
