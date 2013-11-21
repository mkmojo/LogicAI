#!/bin/python
#!/soft/python-2.5-bin/python
print "Content-type:text/html\n\n"

import cgi, cgitb
cgitb.enable()
import socket
import time
import sys
from random import randint

form = cgi.FieldStorage()

def do_welcome():
	f = open("Chat.html")
	html = f.readlines()
	f.close()
	print "".join(html)
	return

def do_send(msg,session,msg_time):
	sock = socket.socket(socket.AF_INET,socket.SOCK_DGRAM)
	address = ('127.0.0.1',12191)
	MESSAGE = "send"  + \
		" " + str(msg_time) + \
		" " + str(session) + \
		" " + str(msg)
	try:
		sock.sendto(MESSAGE, address)
		data,server = sock.recvfrom(65565)
		return data
	finally:
		sock.close()
	return 

def do_login(username,password,session,time_stamp):
	sock = socket.socket(socket.AF_INET,socket.SOCK_DGRAM)
	address = ('127.0.0.1',12191)
	MESSAGE = "logi" + \
		" " + str(username) + \
		" " + str(password) + \
		" " + str(session) + \
		" " + str(time_stamp)
	try:
		sock.sendto(MESSAGE, address)
		data,server = sock.recvfrom(65565)
		return data
	finally:
		sock.close()
	return

def do_logout():
	return "logout_cgi"

def do_query(session,time_stamp):
	sock = socket.socket(socket.AF_INET,socket.SOCK_DGRAM)
	address = ('127.0.0.1',12191)
	MESSAGE = "quer" + \
		" " + str(session) + \
		" " + str(time_stamp)
	try:
		sock.sendto(MESSAGE, address)
		data,server = sock.recvfrom(65565)
		return data
	finally:
		sock.close()
	return


if not form:
	do_welcome()
else:
	action = form.getvalue("action")
	if action == "send":
		message = form.getvalue("content")
		msg_time = form.getvalue("time")
		session = form.getvalue("session")
		print "[" + do_send(message,session,msg_time) + "]"
	elif action == "login":
		username = form.getvalue("username")
		password = form.getvalue("password")
		session = form.getvalue("session")
		time_stamp = form.getvalue("time_stamp")
		if session == "null":
			session = str(randint(1,500))
		print do_login(username,password,session,time_stamp)
	elif action == "query":
		session = form.getvalue("session")
		time_stamp = form.getvalue("time_stamp")
		print "["+do_query(session,time_stamp) + "]"
	else:
		do_logout()
