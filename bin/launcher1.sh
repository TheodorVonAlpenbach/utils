!/bin/bash

adadir=/home/mats/ada
npm=/home/mats/.nvm/versions/node/v22.21.0/bin/npm

gnome-terminal --tab -- bash -c "cd $adadir; emacs -mm --debug-init & echo 'Start Emacs'; exec bash"
gnome-terminal --tab -- bash -c "cd $adadir; exec bash"
gnome-terminal --tab -- bash -c "cd /home/mats/ada/node/admin-api; docker compose up -d activemq elasticsearch kibana mongo mysql nginx-frontend redis-sessions redis; docker compose ps; sleep 1s; $npm start & echo 'Job finished'; exec bash"
gnome-terminal --tab -- bash -c "cd $adadir/node/admin-web; sleep 1s; $npm start & echo 'Job finished. Shell remains open.'; exec bash"
gnome-terminal --tab -- bash -c "cd $adadir/node/portal-web; $npm start & echo 'Job started in background. Use jobs/fg/bg commands to manage it.'; exec bash"
gnome-terminal --tab -- bash -c "cd $adadir/java; exec bash"


