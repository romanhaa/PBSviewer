# PBSviewer

Shiny app that can visualize details of PBS job scheduler.

## How to run

Download/clone repository, navigate inside the main folder and run:

```r
shiny::runApp("PBSviewer.R")
```

## Requirements

* `shiny`
* `shinydashboard`
* `plotly`
* `formattable`
* `tidyverse`

## Does it work with my scheduler?

Might be adaptable to similar job scheduler with some work.

### Expected data from scheduler

List of current jobs.

```
Job id            Name             User              Time Use S Queue
----------------  ---------------- ----------------  -------- - -----
job.1             STDIN            user1             xx:xx:xx R turbovnc
job.2             align_STAR       user2             xx:xx:xx R workq
```

Overview of nodes.

```
                                                        mem       ncpus   nmics   ngpus
vnode           state           njobs   run   susp      f/t        f/t     f/t     f/t   jobs
--------------- --------------- ------ ----- ------ ------------ ------- ------- ------- -------
node01          free                 2     2      0   36gb/126gb    8/28     0/0     0/0 job.1,job.2
node02          free                 0     0      0  126gb/126gb   11/28     0/0     0/0 --
```

Job details.

```
Job Id: <job_id>
    Job_Name = <job_name>
    Job_Owner = <user>@<host>.cluster.local
    resources_used.cpupercent = 264
    resources_used.cput = 00:23:21
    resources_used.mem = 55966724kb
    resources_used.ncpus = 16
    resources_used.vmem = 160963084kb
    resources_used.walltime = 01:30:22
    job_state = R
    queue = workq
    server = <host>.cluster.loc
    Checkpoint = u
    ctime = Tue May  7 09:31:20 2019
    Error_Path = <host>.cluster.loc:<some_path>
    exec_host = <node>/0*16
    exec_vnode = (<node>:ncpus=16)
    Hold_Types = n
    Join_Path = oe
    Keep_Files = n
    Mail_Points = a
    mtime = Tue May  7 09:31:21 2019
    Output_Path = <host>.cluster.loc:<some_path>
    Priority = 0
    qtime = Tue May  7 09:31:20 2019
    Rerunable = True
    Resource_List.mpiprocs = 16
    Resource_List.ncpus = 16
    Resource_List.nodect = 1
    Resource_List.nodes = 1:ppn=16
    Resource_List.place = scatter
    Resource_List.select = 1:ncpus=16:mpiprocs=16
    stime = Tue May  7 09:31:21 2019
    session_id = 15962
    jobdir = <some_path>
    substate = 42
    Variable_List = <...>
    comment = Job run at Tue May 07 at 09:31 on (cn02:ncpus=16)
    etime = Tue May  7 09:31:20 2019
    umask = 22
    run_count = 1
    Submit_arguments = <...>
    project = _pbs_project_default
```

Parsing of the output is tricky since it relies on splitting the output into correct columns.
The problem is that long strings will be wrapped into new lines and not correctly identified later.
Therefore, currently it's only possible to grab features for each job that are not wrapped into a new line.
The `Variable_List` entry is notoriously long which will break string splitting and so all lines below are removed.
Column widths are identified dynamically but there are multiple instances where system-specific strings are removed.
These places might have to be adjusted, including:

* Units (`kb`,`mb`,`gb`).
* `hpcfe01` might show up here and there as that's our server.
