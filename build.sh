mkdir -p diagrams pdf

doit() {
    dir=$1

    stack exec arborism $dir 2>&1 | tee "$dir-subproblems.txt" | grep -- '->' | sort | uniq > "$dir-edges.txt"

    cat "$dir-edges.txt" | cut -d\[ -f1 | tr -d '> ' | tr '-' "\n" | sort | uniq > "$dir-forests.txt"

    ( 
        echo "digraph {" ;
        cat "$dir-forests.txt" | sed -Ee 's/"(([a-z0-9]{6})[a-z0-9]+)"/"\1" [ label = "\2" ];/' ;
        cat "$dir-edges.txt" ;
        echo " } ";
    ) > "diagrams/$dir-subproblems.dot"
}




stack build

doit left

doit right

for i in diagrams/*.dot; do
    dot -Tpdf -o "pdf/$(echo $i | cut -d/ -f2 | cut -d. -f1).pdf" "$i"
done
