

-define(DEBUG(Msg), borkbork:debug_msg(?MODULE, ?LINE, Msg)).


-record(drakon, {
    name,
    parameters,

    diagram, % primitive | silhouette

    attributes = #{}
}).

-record(primitive, {
    skewer,

    attributes = #{}
}).

-record(silhouette, {
    branches = [],

    attributes = #{}
}).

-record(skewer, {
    list = [], % The sequence of items on the skewer.

    attributes = #{}
}).

-record(branch, {
    icon_content,
    skewer,
    address,

    attributes = #{}
}).

-record(question, {
    icon_content,

    right_label,
    right_skewer,
    right_address,

    attributes = #{}
}).

-record(action, {
    icon_content,

    attributes = #{}
}).

-record(insertion, {
    icon_content,

    attributes = #{}
}).

-record(address, {
    icon_content,

    attributes = #{}
}).

-record(link, {
    identifier,

    attributes = #{}
}).

-record(connector, {
    identifier,

    attributes = #{}
}).