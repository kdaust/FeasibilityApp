library(analogsea)
Sys.setenv(DO_PAT="eae4166ed2fac0e3c41660fe26a009bb0176ab8bceeaf753faf5189f58a06520")

temp <- analogsea::droplets()
server <- temp$`shiny-server`
analogsea::droplet_ssh(server, "mkdir /srv/shiny-server/feasibility")
analogsea::droplet_upload(server, "./app/index.Rmd", "/srv/shiny-server/cciss/index.Rmd")
analogsea::droplet_upload(server, "./app/www", "/srv/shiny-server/cciss")
analogsea::droplet_upload(server, "./app/server", "/srv/shiny-server/cciss")
analogsea::droplet_ssh(server, "chown -R shiny:shiny /srv/shiny-server")
analogsea::droplet_ssh(server, "systemctl restart shiny-server")