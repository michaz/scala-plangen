package util;

import code.snippet.LabelledSegment;
import data.mongo.LatLong;
import org.jgrapht.Graphs;
import org.jgrapht.UndirectedGraph;
import org.jgrapht.alg.ConnectivityInspector;
import org.jgrapht.alg.KruskalMinimumSpanningTree;
import org.jgrapht.graph.DefaultWeightedEdge;
import org.jgrapht.graph.SimpleWeightedGraph;
import org.jgrapht.graph.UndirectedSubgraph;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

/**
 * Created with IntelliJ IDEA.
 * User: zilske
 * Date: 8/8/12
 * Time: 6:25 PM
 * To change this template use File | Settings | File Templates.
 */
public class Clusterer {

    public static List<Set<LabelledSegment>> findSignificantLocations(List<LabelledSegment> segments) {


        // Idiotische Art, in quadratischer Zeit den euklidischen MST zu berechnen.
        // Das natürlich beizeiten durch O(nlogn)-Algorithmus ersetzen.
        UndirectedGraph<LabelledSegment, DefaultWeightedEdge> g;
        g = new SimpleWeightedGraph<LabelledSegment, DefaultWeightedEdge>(DefaultWeightedEdge.class);
        for (LabelledSegment segment : segments) {
            g.addVertex(segment);
        }
        for (int i=0; i < segments.size(); ++i) {
            for (int j=i+1; j< segments.size(); ++j) {
                LabelledSegment v1 = segments.get(i);
                LabelledSegment v2 = segments.get(j);
                Graphs.addEdge(g, v1, v2, LatLong.calcDistance(v1.segment().locations().head().location(), v2.segment().locations().head().location()));
            }
        }
        KruskalMinimumSpanningTree<LabelledSegment, DefaultWeightedEdge> mst = new KruskalMinimumSpanningTree<LabelledSegment, DefaultWeightedEdge>(g);

        Set<DefaultWeightedEdge> edges = mst.getEdgeSet();
        System.out.println("Weights:");
        for (DefaultWeightedEdge edge : edges) {
            System.out.println(g.getEdgeWeight(edge));
        }

        Set<DefaultWeightedEdge> shortEdges = new HashSet<DefaultWeightedEdge>();
        for (DefaultWeightedEdge edge : edges) {
            if (g.getEdgeWeight(edge) < 200) {
                shortEdges.add(edge);
            }
        }

        UndirectedSubgraph<LabelledSegment, DefaultWeightedEdge> subgraph = new UndirectedSubgraph<LabelledSegment, DefaultWeightedEdge>(g, g.vertexSet(), shortEdges);
        ConnectivityInspector<LabelledSegment, DefaultWeightedEdge> connectivityInspector = new ConnectivityInspector<LabelledSegment, DefaultWeightedEdge>(subgraph);
        List<Set<LabelledSegment>> clusters = connectivityInspector.connectedSets();


        return clusters;
    }

}
