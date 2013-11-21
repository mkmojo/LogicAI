#!/bin/python
import socket
from datetime import datetime
DISPLAY = []

#online session number
ssn = dict()

user_from_ssn = dict()

user_pass = {'qqy':'111','hjx':'222','wzs':'333','millie':'1234'}

UDPSock = socket.socket(socket.AF_INET,socket.SOCK_DGRAM)
server_addr = ('',12191)
UDPSock.bind(server_addr)

def check_session(session,msg_time):
	if not ssn.has_key(session):
		return False

	oldtime = float( ssn[session] )
	cnttime = float( msg_time )
	five_minutes = 300000
	if(cnttime-oldtime > five_minutes):
		del ssn[session]
		del user_from_ssn[session]
		return False
	else:
		ssn[session] = msg_time
		return True

def construct_entry(name,msg,msg_time):
	Obj_datetime = datetime.fromtimestamp(float(msg_time)/1000)
	hour = Obj_datetime.hour
	minute = Obj_datetime.minute
	sec = Obj_datetime.second
	str_time = str(hour) + ":" + str(minute) + ":" + str(sec)

	return "[" + str_time + "]" + " " + name  + " : " + msg

def do_send(data,addr):
#	DISPLAY.append(data.strip())
	unpack = data.split()
	msg_time = unpack[0]
	session = unpack[1]
	msg = unpack[2]

	active = False
	active = check_session(session,msg_time)

	if active:
		for i in range(3,len(unpack)):
			msg += " " +  unpack[i]
		entry = construct_entry(user_from_ssn[str(session)],msg,msg_time)
		DISPLAY.append(entry)

		CHAT = ""
		start = 0
		for i in range(start,len(DISPLAY)):
			CHAT += "'" + DISPLAY[i] + "'" + ','
		MESSAGE = CHAT
	else:
		MESSAGE = "'" + "not logged in" + "',"

	if data:
		UDPSock.sendto(MESSAGE,addr)
	return

def update_session(usr_in,session_in,time_stamp):
	ssn[session_in] = time_stamp
	user_from_ssn[session_in] = usr_in
	return


def do_login(data,addr):
	unpack = data.split()
	usr_in = unpack[1]
	psw_in = unpack[2]
	session_in = unpack[3]
	time_stamp = unpack[4]
	if user_pass.has_key(usr_in) and user_pass[usr_in] == psw_in:
		update_session(usr_in,session_in,time_stamp)
		MESSAGE = session_in
	else:
		MESSAGE = "failed"

	if data:
		UDPSock.sendto(MESSAGE,addr)
	return

def do_logout():
	return "logout"


def do_query(data,addr):
	unpack = data.split()
	session_in = unpack[1]
	time_stamp = unpack[2]

	active = False
	active = check_session(session_in,time_stamp)

	if active:
		CHAT = ""
		start = 0
		for i in range(start,len(DISPLAY)):
			CHAT += "'" + DISPLAY[i] + "'" + ','
		MESSAGE = CHAT
	else:
		MESSAGE = "'" + "please log in again" + "',"
	if data:
		UDPSock.sendto(MESSAGE,addr)
	return

while True:
        data,addr = UDPSock.recvfrom(65565)#Buffer size is 1024 bytes
        print data.strip(),addr
	action = data[0:4]
	if action == "send":
		do_send(data[5:len(data)],addr)
	elif action == "logi":
		do_login(data,addr)
	elif action == "quer":
		do_query(data,addr)
	else:
		do_logout()
