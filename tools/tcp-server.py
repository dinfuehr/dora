import socket
import os

socket = socket.socket(socket.AF_INET, socket.SOCK_STREAM)  
socket.bind(('127.0.0.1', 12345))  
socket.listen(5)  

while True:  
    conn, _ = socket.accept()
    conn.send("das ist ein test".encode('utf-8'))
    conn.close()

