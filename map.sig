
signature MAP =
sig
    type k
    type 'v map

    val empty : 'v map
    val insert : k -> 'v -> 'v map -> 'v map
    val lookup : k -> 'v map -> 'v option
    val delete : k -> 'v map -> 'v map
    val to_list : 'v map -> (k * 'v) list
    val from_list : (k * 'v) list -> 'v map
end
