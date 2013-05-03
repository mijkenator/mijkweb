#!/bin/bash

sess=mijkweb


tmux list-session 2>&1 | grep -q "^$sess:" || tmux new-session -s $sess -d
tmux list-window -t $sess 2>&1 | grep -q ": polling \[" || tmux new-window -t $sess -n polling
tmux send-keys -t $sess:polling 'cd /home/ashim/work/mijkweb' Enter
tmux split-window -h -t $sess:polling 
tmux split-window -v -t $sess:polling
tmux split-window -v -t $sess:polling.0

tmux send-keys -t $sess:polling.0 'cd /home/ashim/work/mijkweb/test' Enter
tmux send-keys -t $sess:polling.1 'cd /home/ashim/work/mijkweb/test' Enter
tmux send-keys -t $sess:polling.2 'cd /home/ashim/work/mijkweb/test' Enter
tmux send-keys -t $sess:polling.3 'cd /home/ashim/work/mijkweb/test' Enter
#tmux set-window-option -t $sess:polling synchronize-panes on



tmux list-window -t $sess 2>&1 | grep -q ": vim \[" || tmux new-window -t $sess -n vim
tmux send-keys -t $sess:vim 'cd /home/ashim/work/mijkweb' Enter
tmux send-keys -t $sess:vim 'vim -c ":so mijkweb"' Enter


tmux list-window -t $sess 2>&1 | grep -q ": release \[" || tmux new-window -t $sess -n release
tmux send-keys -t $sess:release 'cd /home/ashim/work/mijkweb' Enter
tmux send-keys -t $sess:release 'export LD_LIBRARY_PATH=/opt/libmemcached/lib/' Enter
tmux send-keys -t $sess:release 'sudo service mysqld start' Enter
tmux send-keys -t $sess:release 'sudo service nginx start' Enter
tmux send-keys -t $sess:release 'sudo /etc/init.d/memcached start' Enter
tmux send-keys -t $sess:release './makeall.ch' Enter
tmux send-keys -t $sess:release 'rel/mijkweb/bin/mijkweb console' Enter

tmux list-window -t $sess 2>&1 | grep -q ": compile \[" || tmux new-window -t $sess -n compile
tmux send-keys -t $sess:compile 'cd /home/ashim/work/mijkweb' Enter

tmux list-window -t $sess 2>&1 | grep -q ": nginx \[" || tmux new-window -t $sess -n nginx
tmux send-keys -t $sess:nginx 'cd /opt/mijkweb_pub/html' Enter
tmux send-keys -t $sess:nginx 'vim -c ":so nginx"' Enter

tmux list-window -t $sess 2>&1 | grep -q ": int \[" || tmux new-window -t $sess -n int
tmux send-keys -t $sess:nginx 'cd /opt/mijkweb_pub/html' Enter

tmux -2 attach-session -t $sess
