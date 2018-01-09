-module(sourcer_project).

-record(project, {
    root,
    sources=[],
    includes=[],
    projects=[]
}).
