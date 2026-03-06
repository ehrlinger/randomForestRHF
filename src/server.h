#ifndef RF_SERVER_H
#define RF_SERVER_H
#define SG_TCP_BACKLOG 32
#define SG_TCP_CTRL_NAK   0x00 
#define SG_TCP_CTRL_ACK   0x01 
#define SG_TCP_CTRL_QRY   0x02 
#define SG_TCP_CTRL_NSZ   0x03 
#define SG_TCP_CTRL_TSZ   0x04 
#define SG_TCP_CTRL_EOF   0x05 
#define SG_TCP_CTRL_EOD   0x06 
#define SG_TCP_CTRL_STP   0x07 
#define SG_SOCK_STATE_OPN  0x11
#define SG_SOCK_STATE_LEN  0x12
#define SG_SOCK_STATE_DTA  0x13
void server(uint port, time_t userTimeout, uint xSize, uint pSize, DescriptorObj *headDO);
int serverSend(int clientSockFD, char *record, int length);
int flushReadBuffer(int clientSockFD, DescriptorObj *currDO, char *closeConn);
extern void predictForestRT(DescriptorObj *headDO);
#endif
