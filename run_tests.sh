service postgresql start
sudo -u postgres createuser 'gobblet_server_user'
sudo -u postgres echo 'host all gobblet_server_user 127.0.0.1/32 trust' > /etc/postgresql/9.5/main/pg_hba.conf
service postgresql restart

cd ~/gobblet/src && psql gobblet -U gobblet_server_user < dao/initdb.sql
