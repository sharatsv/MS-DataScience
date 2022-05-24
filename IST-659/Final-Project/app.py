import flask
import sqlite3
from flask import request, jsonify, render_template
from sqlite3 import Error
from collections import OrderedDict
import os

app = flask.Flask(__name__)
app.config["DEBUG"] = True
return_codes = {'insert_success': ('201', 'Insert Successful\n'),
                'update_success': ('201', 'Update Successful\n')
}

banner = "'''<h1> Scale Dashboard (under construction..) </h1>'''"

class SqlLibTestInfra(object):
    def __init__(self):
        self.conn = sqlite3.connect('db/testinfra.db', check_same_thread=False)
        _user_tables = ('nfs_info', 'production', 'server', 'server_make',
                'fault_ticket', 'switch_port_mapping', 'switch',
                'server_switch_info', 'vlan_mapping', 'vlan')
        _res = self.conn.execute('select name from sqlite_master where \
                type=\'table\' and name NOT LIKE \'sqlite_%\';')
        db_tables = _res.fetchall()
        for user_table in _user_tables:
            table_present = False
            for db_table in db_tables:
                if user_table == db_table[0]:
                    table_present = True
                    break
            if not table_present:
                if user_table == 'nfs_info':
                    self.conn.execute('create table nfs_info (\
                            nfs_id int PRIMARY KEY, \
                            nfs_ip_addr varchar(15) not null);')
                if user_table == 'production':
                    self.conn.execute('create table production (\
                            production_id int PRIMARY KEY, \
                            production_name varchar(2) not null, \
                            nfs_share_fqdn varchar(30) not null, \
                            outer_vc_ip varchar(15) not null, \
                            inner_vc_ip varchar(15) not null, \
                            description varchar(15) not null, \
                            runner_ip varchar(15) not null, \
                            nfs_id int not null, \
                            FOREIGN KEY (nfs_id) \
                              REFERENCES nfs_info(nfs_id));')
                if user_table == 'server':
                    self.conn.execute('create table server (\
                            server_name varchar(30) PRIMARY KEY,\
                            server_make_id int, \
                            ip_addr varchar(15), \
                            nfs_vmk_ip_addr varchar(15), \
                            in_use varchar(10), \
                            description varchar(20), \
                            production_id int not null, \
                            FOREIGN KEY (production_id) \
                              REFERENCES production(production_id));')
                if user_table == 'server_make':
                    self.conn.execute('create table server_make (\
                            server_make_id int PRIMARY KEY, \
                            manufacturer varchar(20) not null, \
                            memory_in_gb varchar(3) not null, \
                            cpu_cores varchar(20) not null);')
                if user_table == 'fault_ticket':
                    self.conn.execute('create table fault_ticket (\
                            ticket_id varchar(20) PRIMARY KEY, \
                            description varchar(15) not null, \
                            severity varchar(5) not null, \
                            server_name varchar(30) not null, \
                            FOREIGN KEY (server_name) \
                              REFERENCES server(server_name));')
                if user_table == 'switch_port_mapping':
                    self.conn.execute('create table switch_port_mapping(\
                            switch_port_id int PRIMARY KEY, \
                            port varchar(20) not null);')
                if user_table == 'switch':
                    self.conn.execute('create table switch(\
                            switch_id int PRIMARY KEY, \
                            manufacturer varchar(20) not null, \
                            port_type varchar(20) not null, \
                            ip_addr varchar(15) not null, \
                            switch_type varchar(15) not null);')
                if user_table == 'server_switch_info':
                    self.conn.execute('create table server_switch_info(\
                            server_switch_id int PRIMARY KEY, \
                            server_name varchar(20) not null, \
                            switch_port_id int not null, \
                            FOREIGN KEY (server_name) \
                              REFERENCES server(server_name), \
                            FOREIGN KEY (switch_port_id) \
                              REFERENCES switch_port_mapping(switch_port_id));')
                if user_table == 'vlan':
                    self.conn.execute('create table vlan(\
                            vlan_id int PRIMARY KEY, \
                            description varchar(20) not null, \
                            vlan_type varchar(15) not null);')
                if user_table == 'vlan_mapping':
                    self.conn.execute('create table vlan_mapping(\
                            vlan_bridge_id int PRIMARY KEY, \
                            vlan_id int not null, \
                            production_id int not null, \
                            switch_id int not null, \
                            FOREIGN KEY (vlan_id) \
                              REFERENCES vlan(vlan_id), \
                            FOREIGN KEY (production_id) \
                              REFERENCES production(production_id), \
                            FOREIGN KEY (switch_id) \
                              REFERENCES switch(switch_id));')

    def insert(self, table, data_in_json):
        if table == 'nfs_info':
            _nfs_id = data_in_json['nfs_id']
            _nfs_ip_addr = data_in_json['nfs_ip_addr']
            try:
                self.conn.execute('insert into nfs_info (nfs_id, nfs_ip_addr) \
                        VALUES("%d", "%s");' %(_nfs_id, _nfs_ip_addr))
                self.conn.commit()
                return (return_codes['insert_success'][1],
                        return_codes['insert_success'][0])
            except Error as e:
                return (str(e) + '\n', 500)

        if table == 'production':
            _prod_id = int(data_in_json['production_id'])
            _prod_name = data_in_json['production_name']
            _nfs_share_fqdn = data_in_json['nfs_share_fqdn']
            _outer_vc_ip = data_in_json['outer_vc_ip']
            _inner_vc_ip = data_in_json['inner_vc_ip']
            try:
                _desc = data_in_json['description']
            except KeyError:
                _desc = ''
            _runner_ip = data_in_json['runner_ip']
            _nfs_id = int(data_in_json['nfs_id'])
            try:
                self.conn.execute('insert into production (production_id, \
                        production_name, nfs_share_fqdn, outer_vc_ip, \
                        inner_vc_ip, description, runner_ip, nfs_id)  \
                        VALUES("%s", "%s", "%s", "%s", "%s", "%s", "%s", "%s");' \
                        %(_prod_id, _prod_name, _nfs_share_fqdn, _outer_vc_ip,
                            _inner_vc_ip, _desc, _runner_ip, _nfs_id))
                self.conn.commit()
                return (return_codes['insert_success'][1],
                        return_codes['insert_success'][0])
            except Error as e:
                return (str(e) + '\n', 500)


        if table == 'server':
            _server_name = data_in_json['server_name']
            _server_make_id = data_in_json['server_make_id']
            _ip_addr = data_in_json['ip_addr']
            _nfs_vmk_ip_addr = data_in_json['nfs_vmk_ip_addr']
            _in_use = data_in_json['in_use']
            try:
                _desc = data_in_json['description']
            except KeyError:
                _desc = ''
            _prod_id = data_in_json['production_id']
            try:
                self.conn.execute('insert into server (server_name, \
                        server_make_id, ip_addr, nfs_vmk_ip_addr, \
                        in_use, description, production_id)  \
                        VALUES("%s", "%s", "%s", "%s", "%s", "%s", "%s");' \
                        %(_server_name, _server_make_id, _ip_addr, _nfs_vmk_ip_addr,
                            _in_use, _desc, _prod_id))
                self.conn.commit()
                return (return_codes['insert_success'][1],
                        return_codes['insert_success'][0])
            except Error as e:
                return (str(e) + '\n', 500)

        if table == 'server_make':
            _server_make_id = data_in_json['server_make_id']
            _manufacturer = data_in_json['manufacturer']
            _memory_in_gb = data_in_json['memory_in_gb']
            _cpu_cores = data_in_json['cpu_cores']
            try:
                self.conn.execute('insert into server_make (server_make_id, \
                        manufacturer, memory_in_gb, cpu_cores) \
                        VALUES("%s", "%s", "%s", "%s");' \
                        %(_server_make_id, _manufacturer, _memory_in_gb,
                            _cpu_cores))
                self.conn.commit()
                return (return_codes['insert_success'][1],
                        return_codes['insert_success'][0])
            except Error as e:
                return (str(e) + '\n', 500)

        if table == 'fault_ticket':
            _ticket_id = data_in_json['ticket_id']
            try:
                _desc = data_in_json['description']
            except KeyError:
                _desc = ''
            _severity = data_in_json['severity']
            _server_name = data_in_json['server_name']
            try:
                self.conn.execute('insert into fault_ticket (ticket_id, \
                        description, severity, server_name) \
                        VALUES("%s", "%s", "%s", "%s");' \
                        %(_ticket_id, _desc, _severity,
                          _server_name))
                self.conn.commit()
                return (return_codes['insert_success'][1],
                        return_codes['insert_success'][0])
            except Error as e:
                return (str(e) + '\n', 500)

        if table == 'switch_port_mapping':
            _switch_port_id = data_in_json['switch_port_id']
            _port = data_in_json['port']
            try:
                self.conn.execute('insert into switch_port_mapping (switch_port_id, \
                        port) \
                        VALUES("%s", "%s");'\
                        %(_switch_port_id, _port))
                self.conn.commit()
                return (return_codes['insert_success'][1],
                        return_codes['insert_success'][0])
            except Error as e:
                return (str(e) + '\n', 500)

        if table == 'switch':
            _switch_id = data_in_json['switch_id']
            _manufacturer = data_in_json['manufacturer']
            _port_type = data_in_json['port_type']
            _ip_addr = data_in_json['ip_addr']
            _switch_type = data_in_json['switch_type']
            try:
                self.conn.execute('insert into switch (switch_id, \
                        manufacturer, port_type, ip_addr, switch_type) \
                        VALUES("%s", "%s", "%s", "%s", "%s");'\
                        %(_switch_id, _manufacturer, _port_type, _ip_addr,
                            _switch_type))
                self.conn.commit()
                return (return_codes['insert_success'][1],
                        return_codes['insert_success'][0])
            except Error as e:
                return (str(e) + '\n', 500)

        if table == 'server_switch_info':
            _server_switch_id = data_in_json['server_switch_id']
            _server_name = data_in_json['server_name']
            _switch_port_id = data_in_json['switch_port_id']
            try:
                self.conn.execute('insert into server_switch_info (server_switch_id, \
                        server_name, switch_port_id) \
                        VALUES("%s", "%s", "%s");'\
                        %(_server_switch_id, _server_name, _switch_port_id))
                self.conn.commit()
                return (return_codes['insert_success'][1],
                        return_codes['insert_success'][0])
            except Error as e:
                return (str(e) + '\n', 500)

        if table == 'vlan':
            _vlan_id = data_in_json['vlan_id']
            try:
                _desc = data_in_json['description']
            except KeyError:
                _desc = ''
            _vlan_type = data_in_json['vlan_type']
            try:
                self.conn.execute('insert into vlan (vlan_id, description, \
                        vlan_type) \
                        VALUES("%s", "%s", "%s");'\
                        %(_vlan_id, _desc, _vlan_type))
                self.conn.commit()
                return (return_codes['insert_success'][1],
                        return_codes['insert_success'][0])
            except Error as e:
                return (str(e) + '\n', 500)

        if table == 'vlan_mapping':
            _vlan_bridge_id = data_in_json['vlan_bridge_id']
            _vlan_id = data_in_json['vlan_id']
            _prod_id = data_in_json['production_id']
            _switch_id = data_in_json['switch_id']
            try:
                self.conn.execute('insert into vlan_mapping (vlan_bridge_id, \
                        vlan_id, production_id, switch_id) \
                        VALUES("%s", "%s", "%s", "%s");'\
                        %(_vlan_bridge_id, _vlan_id, _prod_id, _switch_id))
                self.conn.commit()
                return (return_codes['insert_success'][1],
                        return_codes['insert_success'][0])
            except Error as e:
                return (str(e) + '\n', 500)

    def get(self, table):
        self.conn.row_factory = sqlite3.Row
        if table == 'nfs_info':
            rows = self.conn.execute('select * from nfs_info;')
        elif table == 'production':
            rows = self.conn.execute('select * from production;')
        elif table == 'server':
            rows = self.conn.execute('select * from server;')
        elif table == 'server_make':
            rows = self.conn.execute('select * from server_make;')
        elif table == 'fault_ticket':
            rows = self.conn.execute('select * from fault_ticket;')
        elif table == 'switch_port_mapping':
            rows = self.conn.execute('select * from switch_port_mapping;')
        elif table == 'switch':
            rows = self.conn.execute('select * from switch;')
        elif table == 'server_switch_info':
            rows = self.conn.execute('select * from server_switch_info;')
        elif table == 'vlan':
            rows = self.conn.execute('select * from vlan;')
        elif table == 'vlan_mapping':
            rows = self.conn.execute('select * from vlan_mapping;')
        _res = OrderedDict()
        _res_list = []
        for row in rows.fetchall():
            _res_list.append(dict(row))
        _res['result_count'] = len(_res_list)
        _res['results'] = _res_list
        return _res

    def custom_queries(self, query_type):
        if query_type == 'GET_SERVERS_PER_PROD':
            _res = self.conn.execute('select production.production_name, ' \
                                     'count(server.server_name) from server ' \
                                     'JOIN production ON ' \
                                     'server.production_id = production.production_id ' \
                                     'GROUP BY production.production_name')
        if query_type == 'GET_SWITCHES_PER_PROD':
            _res = self.conn.execute('select production.production_name, ' \
                                     'count(switch_port_mapping.switch_port_id) ' \
                                     'from server '\
                                     'JOIN production ON '\
                                     'server.production_id = production.production_id '\
                                     'JOIN server_switch_info ON '\
                                     'server.server_name = server_switch_info.server_name ' \
                                     'JOIN switch_port_mapping ON '\
                                     'server_switch_info.switch_port_id = switch_port_mapping.switch_port_id ' \
                                     'GROUP BY production.production_name')
        if query_type == 'GET_SERVER_TYPES':
            _res = self.conn.execute('select server_make.manufacturer || server_make.memory_in_gb ||  server_make.cpu_cores AS server_type, '\
                                     'count(server_make.manufacturer) from server '\
                                     'JOIN server_make on server.server_make_id = server_make.server_make_id '\
                                     'GROUP BY (server_type)')
        return _res.fetchall()


class SqlLibTestInfo(object):
    def __init__(self):
        self.conn = sqlite3.connect('db/testinfo.db', check_same_thread=False)
        _user_tables = ('groups', 'test_results', 'train_results',
                'test_scores')
        _res = self.conn.execute('select name from sqlite_master where \
                type=\'table\' and name NOT LIKE \'sqlite_%\';')
        db_tables = _res.fetchall()
        for user_table in _user_tables:
            table_present = False
            for db_table in db_tables:
                if user_table == db_table[0]:
                    table_present = True
                    break
            if not table_present:
                # Create table in the database
                if user_table == 'test_results':
                    self.conn.execute('create table test_results (\
                            test_id varchar(30) PRIMARY KEY, \
                            test_env varchar(50) not null, \
                            test_case varchar(100) not null,\
                            release varchar(50) not null, \
                            build varchar(10) not null, \
                            result varchar(10) not null, \
                            date datetime not null);')
                if user_table == 'train_results':
                    self.conn.execute('create table train_results (\
                            test_id varchar(30) PRIMARY KEY, \
                            test_env varchar(50) not null, \
                            test_case varchar(100) not null,\
                            release varchar(50) not null, \
                            build varchar(10) not null, \
                            result varchar(10) not null, \
                            date datetime not null);')
                if user_table == 'test_scores':
                    self.conn.execute('create table test_scores (\
                            test_case varchar(100) PRIMARY KEY, \
                            score varchar(100) not null, \
                            test_env varchar(30) not null, \
                            release varchar(50) not null, \
                            build varchar(10) not null);')
    def insert(self, table, data_in_json):
        if table == 'groups':
            group_id = data_in_json['group_id']
            name = data_in_json['name']
            try:
                self.conn.execute('insert into groups (group_id, name) \
                        VALUES("%d", "%s");' %(group_id, name))
                self.conn.commit()
                return (return_codes['insert_success'][1],
                        return_codes['insert_success'][0])
            except Error as e:
                return (str(e) + '\n', 500)
        if table == 'test_results':
            test_id = data_in_json['test_id']
            test_env = data_in_json['test_env']
            test_case = data_in_json['test_case']
            release = data_in_json['release']
            build = data_in_json['build']
            result = data_in_json['result']
            date = data_in_json['date']
            try:
                self.conn.execute('insert into test_results (test_id, test_env,\
                test_case, release, build, result, date) \
                VALUES("%s","%s","%s","%s","%s", "%s", "%s");' \
                %(test_id, test_env, test_case, release, build, result, date))
                self.conn.commit()
                return (return_codes['insert_success'][1],
                        return_codes['insert_success'][0])
            except Error as e:
                return (str(e) + '\n', 500)
        if table == 'train_results':
            test_id = data_in_json['test_id']
            test_env = data_in_json['test_env']
            test_case = data_in_json['test_case']
            release = data_in_json['release']
            build = data_in_json['build']
            result = data_in_json['result']
            date = data_in_json['date']
            try:
                self.conn.execute('insert into train_results (test_id, test_env,\
                test_case, release, build, result, date) \
                VALUES("%s","%s","%s","%s","%s", "%s", "%s");' \
                %(test_id, test_env, test_case, release, build, result, date))
                self.conn.commit()
                return (return_codes['insert_success'][1],
                        return_codes['insert_success'][0])
            except Error as e:
                return (str(e) + '\n', 500)
        if table == 'test_scores':
            test_case = data_in_json['test_case']
            score = data_in_json['score']
            test_env = data_in_json['test_env']
            release = data_in_json['release']
            build = data_in_json['build']
            try:
                self.conn.execute('insert into test_scores (test_case, score, \
                        test_env, release, build) \
                VALUES("%s","%s","%s","%s","%s");' \
                %(test_case, score,test_env,release,build))
                self.conn.commit()
                return (return_codes['insert_success'][1],
                        return_codes['insert_success'][0])
            except Error as e:
                return (str(e) + '\n', 500)


    def get(self, table, my_filter=None, raw_output=False):
        self.conn.row_factory = sqlite3.Row
        if table == 'groups':
            _res = self.conn.execute('select * from groups;')
        if table == 'test_results':
            if my_filter:
                _test_case = my_filter['test_case']
                _build = my_filter['build']
                _release = my_filter['release']
                _test_env = my_filter['test_env']
                rows = self.conn.execute('select test_case, build, release, \
                        test_env, result \
                        from test_results where \
                        test_case="%s" AND \
                        build="%s" AND \
                        release="%s" AND \
                        test_env="%s";' \
                        %(_test_case, _build, _release, _test_env)
                        )
            else:
                rows = self.conn.execute('select test_id, test_env, test_case, \
                        release, build, result from test_results;')
        if table == 'train_results':
            if my_filter:
                _test_case = my_filter['test_case']
                _build = my_filter['build']
                _release = my_filter['release']
                _test_env = my_filter['test_env']
                rows = self.conn.execute('select test_case, build, release, \
                        test_env, result \
                        from train_results where \
                        test_case="%s" AND \
                        build="%s" AND \
                        release="%s" AND \
                        test_env="%s";' \
                        %(_test_case, _build, _release, _test_env)
                        )
            else:
                rows = self.conn.execute('select test_id, test_env, test_case, \
                        release, build, result from train_results;')
        elif table == 'test_scores':
            if my_filter:
                print('select * from test_scores where \
                        test_case="%s";'%(my_filter))
                rows = self.conn.execute('select * from test_scores where \
                        test_case="%s";'%(my_filter))
            else:
                rows = self.conn.execute('select * from test_scores;')
        if raw_output:
            return rows.fetchall()
        _res = OrderedDict()
        _res_list = []
        for row in rows.fetchall():
            _res_list.append(dict(row))
        _res['result_count'] = len(_res_list)
        _res['results'] = _res_list
        return _res


    def update(self, table, data_in_json):
        if table == 'test_scores':
            test_case = data_in_json['test_case']
            score = data_in_json['score']
        try:
            self.conn.execute('update test_scores SET test_case=\"%s\", \
            score=\"%s\" where test_case=\"%s\";' %(test_case, score, test_case))
            return (return_codes['update_success'][1],
                    return_codes['update_success'][0])
        except Error as e:
            return(str(e) + '\n', 500)

    def custom_queries(self, query_type):
        if query_type == 'GET_ALL_RESULTS':
            _res = self.conn.execute('select * from test_results ' \
                                     'ORDER BY date DESC ' \
                                     'LIMIT 500')
            return _res.fetchall()
        if query_type == 'GET_RESULTS_PER_RELEASE':
            _res1 = self.conn.execute('select Release, count(test_case) from test_results ' \
                                     'where result=\'PASS\' GROUP BY Release')
            _res2 = self.conn.execute('select Release, count(test_case) from test_results ' \
                                     'where result=\'FAIL\' GROUP BY Release')
            i, j = zip(*_res1.fetchall())
            _, k = zip(*_res2.fetchall())
            return i, j, k

        if query_type == 'GET_RESULTS_BY_HOUR':
            _res1 = self.conn.execute('select date, count(result) from test_results ' \
                                      'GROUP BY strftime(\'%Y-%m-%d %H\', date) '\
                                      'ORDER BY date desc LIMIT 10')
            i, j = zip(*_res1.fetchall())
            return i, j

        if query_type == 'GET_TRAIN_RESULTS_BY_HOUR':
            _res1 = self.conn.execute('select date, count(result) from train_results ' \
                                      'GROUP BY strftime(\'%Y-%m-%d %H\', date) '\
                                      'ORDER BY date desc LIMIT 10')
            i, j = zip(*_res1.fetchall())
            return i, j

        if query_type == 'GET_RECENT_BUILD_QUALITY':
            _pass = self.conn.execute('select build || \'-\' ||  release, count(result) from test_results ' \
                                      'where result=\'PASS\' GROUP BY release ' \
                                      'ORDER BY build desc LIMIT 15')
            _fail = self.conn.execute('select build || \'-\' || release, count(result) from test_results ' \
                                      'where result=\'FAIL\' GROUP BY release ' \
                                      'ORDER BY build desc LIMIT 15')
            i, p = zip(*_pass.fetchall())
            _, f = zip(*_fail.fetchall())
            quality=[]
            for _p, _f in zip(p, f):
                quality.append(int(_p * 100 / (_p + _f)))
            return i, quality

db_testinfo = SqlLibTestInfo()
db_testinfra = SqlLibTestInfra()

def common_db_crud_ops(table, request):
    if request.method == 'POST':
        if request.is_json:
            status = db_testinfra.insert(table, request.json)
            return status
    if request.method == 'GET':
        data = db_testinfra.get(table)
        return jsonify(data)

@app.route('/api/v1/groups/', methods=['POST', 'GET'])
def test():
    if request.method == 'POST':
        if request.is_json:
            status = db_testinfo.insert('groups', request.json)
            return status
    if request.method == 'GET':
        data = db_testinfo.get('groups')
        return jsonify(data)
    else:
        return "Method %s not implemented" %(request.method)

@app.route('/api/v1/test-results/', methods=['POST', 'GET'])
def scale_test_results():
    if request.method == 'POST':
        if request.is_json:
            status = db_testinfo.insert('test_results', request.json)
            return status
    if request.method == 'GET':
        if request.args:
            my_filter = request.args.to_dict()
            data = db_testinfo.get('test_results', my_filter)
        else:
            data = db_testinfo.get('test_results')
        return jsonify(data)

@app.route('/api/v1/train-results/', methods=['POST', 'GET'])
def scale_train_results():
    if request.method == 'POST':
        if request.is_json:
            status = db_testinfo.insert('train_results', request.json)
            return status
    if request.method == 'GET':
        if request.args:
            my_filter = request.args.to_dict()
            data = db_testinfo.get('train_results', my_filter)
        else:
            data = db_testinfo.get('train_results')
        return jsonify(data)

@app.route('/api/v1/test-scores/', methods=['POST', 'GET', 'PATCH'])
def scale_test_scores():
    if request.method == 'POST':
        if request.is_json:
            status = db_testinfo.insert('test_scores', request.json)
            return status
    if request.method == 'GET':
        if request.args.get('test_case'):
            data = db_testinfo.get('test_scores',
                    my_filter=request.args.get('test_case'))
        else:
            data = db_testinfo.get('test_scores')
        return jsonify(data)
    if request.method == 'PATCH':
        if request.is_json:
            status = db_testinfo.update('test_scores', request.json)
            return status

# Server/switch inventory - apis
@app.route('/api/v1/production/', methods=['POST', 'GET'])
def production():
    return common_db_crud_ops('production', request)

@app.route('/api/v1/nfs_info/', methods=['POST', 'GET'])
def nfs_info():
    return common_db_crud_ops('nfs_info', request)

@app.route('/api/v1/server/', methods=['POST', 'GET'])
def server():
    return common_db_crud_ops('server', request)

@app.route('/api/v1/server_make/', methods=['POST', 'GET'])
def server_make():
    return common_db_crud_ops('server_make', request)

@app.route('/api/v1/fault_ticket/', methods=['POST', 'GET'])
def fault_ticket():
    return common_db_crud_ops('fault_ticket', request)

@app.route('/api/v1/switch_port_mapping/', methods=['POST', 'GET'])
def switch_port_mapping():
    return common_db_crud_ops('switch_port_mapping', request)

@app.route('/api/v1/switch/', methods=['POST', 'GET'])
def switch():
    return common_db_crud_ops('switch', request)

@app.route('/api/v1/server_switch_info/', methods=['POST', 'GET'])
def server_switch_info():
    return common_db_crud_ops('server_switch_info', request)

@app.route('/api/v1/vlan/', methods=['POST', 'GET'])
def vlan():
    return common_db_crud_ops('vlan', request)

@app.route('/api/v1/vlan_mapping/', methods=['POST', 'GET'])
def vlan_mapping():
    return common_db_crud_ops('vlan_mapping', request)

# Bar-chart properties
colors = [
    "#F7464A", "#46BFBD", "#FDB45C", "#FEDCBA",
    "#ABCDEF", "#DDDDDD", "#ABCABC", "#4169E1",
    "#C71585", "#FF4500", "#FEDCBA", "#46BFBD"]

@app.route('/test-servers')
def chart():
    servers_per_prod = db_testinfra.custom_queries('GET_SERVERS_PER_PROD')
    prod, servers = zip(*servers_per_prod)
    labels_1 = prod
    values_1 = servers
    switches_per_prod = db_testinfra.custom_queries('GET_SWITCHES_PER_PROD')
    prod, switches = zip(*switches_per_prod)
    labels_2 = prod
    values_2 = switches
    server_types = db_testinfra.custom_queries('GET_SERVER_TYPES')
    server_type, count = zip(*server_types)
    labels_3 = server_type
    values_3 = count
    return render_template('chart.html',
                           title_1='SERVERS - Grouped by Production',
                           max_1=int(1.1 * max(values_1)),
                           labels_1=labels_1,
                           values_1=values_1,
                           title_2='SWITCHES - Grouped by Production',
                           max_2=int(1.1 * max(values_2)),
                           labels_2=labels_2,
                           values_2=values_2,
                           title_3='SERVERS - Grouped by H/W Specs',
                           max_3=int(1.1 * max(values_3)),
                           set=zip(values_3, labels_3, colors)
                           )
    #return render_template('index.html')
    data = db_testinfo.get('test_results')
    page = ''
    for val in data:
        page = page + str(val)
    page = banner + "'''<p>" + page + "</p>'''"
    return page

@app.route('/test-results')
def test_analytics():
    data = db_testinfo.custom_queries('GET_ALL_RESULTS')
    # Chart-1
    rel, _pass, _fail = db_testinfo.custom_queries('GET_RESULTS_PER_RELEASE')
    # Chart-2
    build, _quality = db_testinfo.custom_queries('GET_RECENT_BUILD_QUALITY')
    # Chart-3
    date_1, tc_count_1 = db_testinfo.custom_queries('GET_RESULTS_BY_HOUR')
    # Chart-4
    date_2, tc_count_2 = db_testinfo.custom_queries('GET_TRAIN_RESULTS_BY_HOUR')
    return render_template('tables.html',
                           value=data,
                           title_1='RESULTS (All Releases)',
                           labels_1=rel,
                           values_1=_pass,
                           values_1_2=_fail,
                           max_1=int(1.1 * max(_pass)),
                           title_2='RECENT BUILD QUALITY',
                           labels_2=build,
                           values_2=_quality,
                           max_2=int(1.1 * max(_quality)),
                           title_3='#TEST CASES EXECUTED (Hour)',
                           labels_3=date_1,
                           values_3=tc_count_1,
                           max_3=int(1.1 * max(tc_count_1)),
                           title_4='#TEST CASES TRAINED (Hour)',
                           labels_4=date_2,
                           values_4=tc_count_2,
                           max_4=int(1.1 * max(tc_count_2)),
                           )

# App-start
app.run(host='0.0.0.0')
