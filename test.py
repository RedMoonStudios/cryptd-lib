#!/usr/bin/env python
import os
import time
import httplib
import BaseHTTPServer
from subprocess import check_output, Popen
from multiprocessing import Process

CONFIG = {
    'master': "dist/build/cryptd/cryptd",
    'slave': "dist/build/cryptd-slave/cryptd-slave",
    'slave_template': "dist/build/cryptd-slave-template/cryptd-slave-template",
}

class Master(object):
    def ctrl(self, *cmd):
        return check_output([CONFIG['master']] + list(cmd))

    def create_partner(self):
        if not self.ctrl('partner', 'list').startswith('("1",'):
            self.ctrl('partner', 'add', '1', 'http://example.com/')

    def create_arch(self):
        if not self.ctrl('arch', 'list').startswith('"test"'):
            action = 'add'
        else:
            action = 'change'
        self.ctrl('arch', action, 'test', CONFIG['slave_template'])

    def build_slave(self):
        self.create_partner()
        self.create_arch()

        slavedir = os.path.dirname(CONFIG['slave'])
        if not os.path.exists(slavedir):
            os.makedirs(slavedir)
        self.ctrl('build', '1', 'test', CONFIG['slave'])
        os.chmod(CONFIG['slave'], 0777)

    def __enter__(self):
        cmd = [
            CONFIG['master'],
            'serve', '-f'
            '-u', 'http://localhost:1234/'
        ]
        self.handle = Popen(cmd)
        return self.handle

    def __exit__(self, extype, value, traceback):
        self.handle.terminate()

class Slave(object):
    def __init__(self, port, instance=None):
        self.port = port
        self.instance = instance

    def __enter__(self):
        cmd = [
            CONFIG['slave'], '-f',
            '--masterhost=localhost',
            '-p', str(self.port),
            '-u', 'http://localhost:4321/',
        ]
        if self.instance is not None:
            cmd.append(self.instance)
        self.handle = Popen(cmd)
        return self.handle

    def __exit__(self, extype, value, traceback):
        self.handle.terminate()

class APIHandler(BaseHTTPServer.BaseHTTPRequestHandler):
    def do_POST(self):
        print self.headers
        print self.rfile.read(int(self.headers['Content-Length']))

        self.send_response(200)
        self.send_header('Content-Type', 'text/plain')
        self.end_headers()
        self.wfile.write("test_data")
        self.wfile.close()

class APIServer(object):
    def serve_once(self, port, times=1):
        addr = ('localhost', port)
        httpd = BaseHTTPServer.HTTPServer(
            addr, APIHandler
        )
        while times > 0:
            httpd.handle_request()
            times -= 1

    def serve(self, port, times=1):
        p = Process(target=self.serve_once, args=(port, times))
        p.start()
        return p

def murder(proc):
    proc.join(5)
    if proc.is_alive():
        proc.terminate()

def post_test(port, path, notice, slave_id=None, inst=None):
    headers = {
        'Content-Type': 'text/plain',
    }

    if slave_id is not None:
        headers['X-SlaveID'] = str(slave_id)

    if inst is not None:
        headers['X-Instance'] = str(inst)

    conn = httplib.HTTPConnection('localhost', port, timeout=5)
    conn.request('POST', path, notice, headers)
    response = conn.getresponse()
    print response.status, response.reason
    print response.read()
    conn.close()

if __name__ == '__main__':
    m = Master()
    m.build_slave()

    with m, Slave(17771), Slave(18881, "xxx"):
        time.sleep(3)

        api = APIServer()

        srv = api.serve(4321, 3)
        try:
            post_test(16661, '/to4321/x', 'from_master_to_slave', 1)
            post_test(16661, '/to4321/x', 'from_master_to_slave', 1, 'xxx')
        finally:
            murder(srv)

        srv = api.serve(1234)
        try:
            post_test(17771, '/to1234/y', 'from_slave_to_master')
        finally:
            murder(srv)
