FROM rocker/binder:latest
LABEL maintainer='Choonghyun Ryu'
USER root
COPY . ${HOME}
RUN chown -R ${NB_USER} ${HOME}
USER ${NB_USER}



RUN wget https://github.com/bit2r/BitStat/blob/master/DESCRIPTION && R -e "options(repos = list(CRAN = 'http://mran.microsoft.com/snapshot/2021-12-01/')); devtools::install_deps()"

RUN rm DESCRIPTION.1; exit 0
