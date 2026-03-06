
// *** THIS HEADER IS AUTO GENERATED. DO NOT EDIT IT ***
#include           "shared/globalCore.h"
#include           "shared/externalCore.h"
#include           "global.h"
#include           "external.h"

// *** THIS HEADER IS AUTO GENERATED. DO NOT EDIT IT ***

      
    

#include "server.h"
#include "descriptor.h"
#include "shared/nrutil.h"
#include "shared/error.h"
#include <sys/socket.h>
#include <sys/ioctl.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <unistd.h>
#include <errno.h>
void server(uint port, time_t userTimeout, uint xSize, uint pSize, DescriptorObj *headDO) {
  DescriptorObj *tailDO, *currDO;
  char ctrlID;
  int listen_sd, max_sd, new_sd;
  int desc_ready;
  int optval;
  int result;
  uint live;
  struct sockaddr_in addr;
  struct timeval timeout;
  fd_set master_set, working_set;
  uint16_t nboLength, hboLength;
  uint32_t nbo_recordID;
  float hbo_fltValue;
  uint32_t *hbo_intValuePtr;
  char serverExit;
  char closeConn;
  int listen_i;
  uint i, k;
  tailDO = headDO;
  listen_sd = socket(AF_INET, SOCK_STREAM, 0); 
  if (listen_sd < 0) { 
    RF_nativeError("\nRF-SRC:  *** ERROR *** ");
    RF_nativeError("\nRF-SRC:  socket() failed with errno %10d.", errno);
    RF_nativeError("\nRF-SRC:  Please Contact Technical Support.");
    RF_nativeExit();
  } 
  optval = TRUE;
  result = setsockopt(listen_sd, SOL_SOCKET, SO_REUSEADDR, (char *) &optval, sizeof(optval));
  if (result < 0) { 
    close(listen_sd);
    RF_nativeError("\nRF-SRC:  *** ERROR *** ");
    RF_nativeError("\nRF-SRC:  setsockopt() failed with errno %10d.", port, errno);
    RF_nativeError("\nRF-SRC:  Please Contact Technical Support.");
    RF_nativeExit();
  } 
  result = ioctl(listen_sd, FIONBIO, (char *) &optval);
  if (result < 0) { 
    close(listen_sd);
    RF_nativeError("\nRF-SRC:  *** ERROR *** ");
    RF_nativeError("\nRF-SRC:  ioctl() failed with errno %10d.", errno);
    RF_nativeError("\nRF-SRC:  Please Contact Technical Support.");
    RF_nativeExit();
  } 
  memset(&addr, 0, sizeof(addr)); 
  addr.sin_family = AF_INET; 
  addr.sin_addr.s_addr = htonl(INADDR_ANY); 
  addr.sin_port = htons(port); 
  result = bind(listen_sd, (struct sockaddr *) &addr, sizeof(addr));
  if (result < 0) { 
    close(listen_sd);
    RF_nativeError("\nRF-SRC:  *** ERROR *** ");
    RF_nativeError("\nRF-SRC:  bind() failed with errno %10d.", errno);
    RF_nativeError("\nRF-SRC:  Please Contact Technical Support.");
    RF_nativeExit();
  } 
  result = listen(listen_sd, SG_TCP_BACKLOG);
  if (result < 0) { 
    close(listen_sd);
    RF_nativeError("\nRF-SRC:  *** ERROR *** ");
    RF_nativeError("\nRF-SRC:  listen() failed with errno %10d.", errno);
    RF_nativeError("\nRF-SRC:  Please Contact Technical Support.");
    RF_nativeExit();
  } 
  FD_ZERO(&master_set);
  max_sd = listen_sd;
  FD_SET(listen_sd, &master_set);
  timeout.tv_sec  = userTimeout;
  timeout.tv_usec = 0;
  serverExit = FALSE;
  hbo_intValuePtr = (uint32_t *) & hbo_fltValue;
  do {
    memcpy(&working_set, &master_set, sizeof(master_set));
    result = select(max_sd + 1, &working_set, NULL, NULL, &timeout);
    if (result < 0) { 
      break;
    } 
    if (result == 0) {
      headDO -> userState = SG_DESC_CLOSED;
      break;
    }
    desc_ready = result;
    for (listen_i = 0; listen_i <= max_sd  &&  desc_ready > 0; ++listen_i) {
      if (FD_ISSET(listen_i, &working_set)) {
        desc_ready -= 1;
        if (listen_i == listen_sd) {
          if (headDO -> userState != SG_DESC_CLOSED) {
            do {
              new_sd = accept(listen_sd, NULL, NULL);
              if (new_sd < 0) {
                if (errno != EWOULDBLOCK) {
                  serverExit = TRUE;
                }
                break;
              }
#ifdef _OPENMP
              omp_set_lock(&SG_lockDO);
#endif
              tailDO -> fwdLink = makeDescriptorObj(xSize, pSize, new_sd, SG_SOCK_STATE_OPN);
              tailDO -> fwdLink -> bakLink = tailDO;
              tailDO = tailDO -> fwdLink;
#ifdef _OPENMP
              omp_unset_lock(&SG_lockDO);
#endif
              FD_SET(new_sd, &master_set);
              if (new_sd > max_sd) {
                max_sd = new_sd;
              }
            } while (new_sd != -1);
          }  
        }  
        else {
          closeConn = FALSE;
          currDO = headDO;
          while (currDO != tailDO) {
            currDO = currDO -> fwdLink;
            if (currDO -> descID == listen_i) {
              break;
            }
          }
          do {
            if (currDO -> socketState == SG_SOCK_STATE_OPN) {
              ctrlID = SG_TCP_CTRL_NAK;
              result = recv(listen_i, &nboLength, sizeof(uint16_t), 0);
              if (result < 0) {
                if (errno != EWOULDBLOCK) {
                  closeConn = TRUE;
                }
                break;
              }
              else if (result == 0) {
                closeConn = TRUE;
              }
              else if (result == 2) {
                if (currDO -> userState == SG_DESC_PREREAD) {
                  currDO -> nSizeAlloc = ntohs(nboLength);
                  currDO -> len = 0;
                  currDO -> userState = SG_DESC_READING;
                  ctrlID = currDO -> userState;
                  setDescriptorObj(currDO, currDO -> nSizeAlloc, FALSE);
                }
                else {
                  currDO -> len = ntohs(nboLength);
                }
                if (currDO -> len >= 1) {
                  ctrlID = SG_TCP_CTRL_ACK;
                }
              }
              else {
              }
              result = serverSend(listen_i, &ctrlID, 1);
              if (result < 0) {
                closeConn = TRUE;
              }
              else if (ctrlID == SG_TCP_CTRL_ACK) {
                currDO -> socketState = SG_SOCK_STATE_LEN;
              }
              if (closeConn == TRUE) {
                break;
              }
            }  
            if (currDO -> socketState == SG_SOCK_STATE_LEN) {
              result = recv(listen_i, currDO -> record, currDO -> len, 0);
              if (result < 0) {
                if (errno != EWOULDBLOCK) {
                  closeConn = TRUE;
                }
                break;
              }
              else if (result == 0) {
                closeConn = TRUE;
              }
              else {
                if (result != currDO -> len) {
                  RF_nativeError("\nRF-SRC:  *** USER FORCED ABORT *** ");  RF_nativeExit();
                }
                currDO -> socketState = SG_SOCK_STATE_DTA;
                currDO -> record[currDO -> len] = 0x00;
              }
              if (closeConn == TRUE) {
                break;
              }
            }  
            if (currDO -> socketState == SG_SOCK_STATE_DTA) {
              ctrlID = SG_TCP_CTRL_NAK;
              if (currDO -> len == 1) {
                if (currDO -> record[0] == SG_TCP_CTRL_QRY) {
                  ctrlID = currDO -> userState;
                }
                else if (currDO -> record[0] == SG_TCP_CTRL_NSZ) {
                  if (currDO -> userState == SG_DESC_OPENED) {
                    currDO -> userState = SG_DESC_PREREAD;
                    ctrlID = currDO -> userState;
                  }
                  else {
                  }
                }
                else if (currDO -> record[0] == SG_TCP_CTRL_EOF) {
                  if (currDO -> userState == SG_DESC_READING) {
                    ctrlID = SG_DESC_POSTREAD;
                  }
                  else {
                  }
                }
                else if (currDO -> record[0] == SG_TCP_CTRL_EOD) {
                  if (currDO -> userState == SG_DESC_OPENED) {
                    ctrlID = SG_TCP_CTRL_ACK;
                    closeConn = TRUE;
                  }
                }
                else if ((currDO -> record[0] == '\n') || (currDO -> record[0] == '\r')) {
                  if (currDO -> userState == SG_DESC_READING) {
                    ctrlID = currDO -> userState;
                    currDO -> socketState = SG_SOCK_STATE_OPN;
                    (currDO -> nSizeRead) ++;
                    (currDO -> mSize) ++;
                    (currDO -> mIndx)[currDO -> mSize] = currDO -> nSizeRead;
                  }
                  else {
                  }                  
                }
                else if (currDO -> record[0] == SG_TCP_CTRL_STP) {
                  ctrlID = SG_TCP_CTRL_ACK;
                  closeConn = TRUE;
                  headDO -> userState = SG_DESC_CLOSED;
                }
                else if (currDO -> record[0] == SG_TCP_CTRL_TSZ) {
                  if (currDO -> userState == SG_DESC_POSTPREDICTING) {
                    hboLength = currDO -> nSize;
                    nboLength = htons(hboLength);
                    result = send(listen_i, &nboLength, sizeof(uint16_t), 0);
                    if (result == -1) {
                    }
                    else if (result == 0) {
                    }
                    else if (result == sizeof(uint16_t)) {
                      currDO -> userState = SG_DESC_PREWRITING;
                    }
                    else {
                    }
                  }
                }
                else {
                }
                if (!(currDO -> record[0] == SG_TCP_CTRL_TSZ)) {
                  result = serverSend(listen_i, &ctrlID, 1);
                  if (result < 0) {
                    closeConn = TRUE;
                  }
                }
                if (((currDO -> record[0] == SG_TCP_CTRL_QRY) && ((currDO -> userState) == SG_DESC_PREWRITING))) {
                  currDO -> userState = SG_DESC_WRITING;
                }
                if (((currDO -> record[0] == SG_TCP_CTRL_QRY) && ((currDO -> userState) == SG_DESC_WRITING))) {
                  (currDO -> nSent) ++;
                  nbo_recordID = htonl(currDO -> nIndx[currDO -> nSent]);
                  currDO -> yHatPacket -> recordID = nbo_recordID;
                  hbo_fltValue = (float) currDO -> yHat[currDO -> nSent];
                  currDO -> yHatPacket -> intValue = htonl(*hbo_intValuePtr);
                  send(listen_i, (char *) (currDO -> yHatPacket), sizeof(currDO -> yHatPacket), 0);
                  if (currDO -> nSent == currDO -> nSize) {
                    resetDescriptorObj(currDO, SG_SOCK_STATE_OPN);
                  }
                }
                if (((currDO -> record[0] == SG_TCP_CTRL_EOF) && ((currDO -> userState) == SG_DESC_READING))) {
#ifdef _OPENMP
                  omp_set_lock(&SG_lockDO);
#endif
                  currDO -> userState = SG_DESC_POSTREAD;
#ifdef _OPENMP
                  omp_unset_lock(&SG_lockDO);
#endif
                }
              }  
              else {
                if (currDO -> userState == SG_DESC_READING) {
                  (currDO -> nSizeRead) ++;
                  char *oldPtr, *newPtr;
                  char recordExit;
                  uint count;
                  recordExit = FALSE;
                  oldPtr = currDO -> record;
                  count = 0;
                  double *xRow = dvector(1, currDO -> pSize);
                  while (!recordExit && (*oldPtr) && (*oldPtr != '\r') && (*oldPtr != '\n')) {
                    xRow[++count] = strtod(oldPtr, &newPtr);  
                    if (newPtr == oldPtr) {
                      recordExit = TRUE;
                    }
                    while (*newPtr == ' ') newPtr++; 
                    if (*newPtr == ',') {
                      newPtr ++;
                    }
                    oldPtr = newPtr;
                  }
                  if (count == currDO -> pSize) {
                    (currDO -> nSize) ++;
                    (currDO -> nIndx)[currDO -> nSize] = currDO -> nSizeRead;
                    ctrlID = currDO -> userState;
                    for (i = 1; i <= currDO -> xSize; i++) {
                      currDO -> fxArrayIn[i][currDO -> nSize] = xRow[i];
                    }
                    for (i = 1; i <= currDO -> augmSize; i++) {
                      currDO -> fxArrayInAugm[i][currDO -> nSize] = xRow[currDO -> xSize + i];
                    }
                  }
                  else {
                    (currDO -> mSize) ++;
                    (currDO -> mIndx)[currDO -> mSize] = currDO -> nSizeRead;
                  }
                  free_dvector(xRow, 1, currDO -> pSize);
                }  
                else {
                }
                result = serverSend(listen_i, &ctrlID, 1);
                if (result < 0) {
                  closeConn = TRUE;
                }
              }  
              if (closeConn == TRUE) {
                break;
              }
              currDO -> socketState = SG_SOCK_STATE_OPN;
            }  
          } while (TRUE);
#ifndef _OPENMP
          if ((closeConn == FALSE) && (currDO -> userState == SG_DESC_POSTREAD)) {
            predictForestRT(headDO);
          }
#endif
          if (closeConn == TRUE) {
            currDO -> descID = 0;
            close(listen_i);
            FD_CLR(listen_i, &master_set);
            if (listen_i == max_sd) {
              while (FD_ISSET(max_sd, &master_set) == FALSE) {
                max_sd -= 1;
              }
            }
          }
          else {
          }
#ifdef _OPENMP
          omp_set_lock(&SG_lockDO);
#endif
          k = unlinkUnheldDescriptors(headDO, &tailDO, FALSE, &live);
#ifdef _OPENMP
          omp_unset_lock(&SG_lockDO);
#endif
          if (headDO -> userState == SG_DESC_CLOSED) {
            if (live == 0) {
              serverExit = TRUE;
              break;
            }
          }
        }  
      }  
    }  
  } while (serverExit == FALSE);
  for (listen_i = 0; listen_i <= max_sd; ++listen_i) {
    if (FD_ISSET(listen_i, &master_set)) {
      close(listen_i);
    }
  }
  while (headDO != tailDO) {
#ifdef _OPENMP
    omp_set_lock(&SG_lockDO);
#endif
    k = unlinkUnheldDescriptors(headDO, &tailDO, TRUE, &live);
#ifdef _OPENMP
    omp_unset_lock(&SG_lockDO);
#endif
  }
}
int serverSend(int clientSockFD, char *record, int length) {
  int recordLength;
  recordLength = send(clientSockFD, record, length, 0);
  if (recordLength == -1) {
    RF_nativeError("\nRF-SRC:  *** ERROR *** ");
    RF_nativeError("\nRF-SRC:  Unsuccessful send to client %10d with errno %10d.", clientSockFD, errno);
    RF_nativeError("\nRF-SRC:  Please Contact Technical Support.");
  }
  return recordLength;
}
int flushReadBuffer(int clientSockFD, DescriptorObj *currDO, char *closeConn) {
  int result;
  do {
    result = recv(clientSockFD, currDO -> record, 1, 0);
    if (result < 0) {
      break;
    }
  } while (TRUE);
  return result;
}
