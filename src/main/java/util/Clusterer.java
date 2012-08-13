package util;

import org.geotools.referencing.GeodeticCalculator;
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

    public interface ThingToCluster {
        double getLat();
        double getLong();
    }

    static double calcDistance(double lat1, double long1, double lat2, double long2) {
        GeodeticCalculator gc = new GeodeticCalculator();
        gc.setStartingGeographicPoint(lat1, long1);
        gc.setDestinationGeographicPoint(lat2, long2);
        return gc.getOrthodromicDistance();
    }

    public static <A extends ThingToCluster> List<Set<A>> findSignificantLocations(List<A> segments) {


        // Idiotische Art, in quadratischer Zeit den euklidischen MST zu berechnen.
        // Das natürlich beizeiten durch O(nlogn)-Algorithmus ersetzen.
        UndirectedGraph<A, DefaultWeightedEdge> g;
        g = new SimpleWeightedGraph<A, DefaultWeightedEdge>(DefaultWeightedEdge.class);
        for (A segment : segments) {
            g.addVertex(segment);
        }
        for (int i=0; i < segments.size(); ++i) {
            for (int j=i+1; j< segments.size(); ++j) {
                A v1 = segments.get(i);
                A v2 = segments.get(j);
                Graphs.addEdge(g, v1, v2, calcDistance(v1.getLat(), v1.getLong(), v2.getLat(), v2.getLong()));
            }
        }
        KruskalMinimumSpanningTree<A, DefaultWeightedEdge> mst = new KruskalMinimumSpanningTree<A, DefaultWeightedEdge>(g);

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

        UndirectedSubgraph<A, DefaultWeightedEdge> subgraph = new UndirectedSubgraph<A, DefaultWeightedEdge>(g, g.vertexSet(), shortEdges);
        ConnectivityInspector<A, DefaultWeightedEdge> connectivityInspector = new ConnectivityInspector<A, DefaultWeightedEdge>(subgraph);
        List<Set<A>> clusters = connectivityInspector.connectedSets();


        return clusters;
    }

}
