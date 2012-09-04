package util;

import org.geotools.referencing.GeodeticCalculator;
import org.jgrapht.Graphs;
import org.jgrapht.UndirectedGraph;
import org.jgrapht.alg.ConnectivityInspector;
import org.jgrapht.alg.KruskalMinimumSpanningTree;
import org.jgrapht.graph.DefaultWeightedEdge;
import org.jgrapht.graph.SimpleWeightedGraph;
import org.jgrapht.graph.UndirectedSubgraph;

import java.util.ArrayList;
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
        return findSignificantLocations(segments, 400.0);
    }

    private static <A extends ThingToCluster> List<Set<A>> findSignificantLocations(List<A> segments, double length) {
        List<Set<A>> clusters = new ArrayList<Set<A>>();
        for (A segment: segments) {
            boolean hasGoneInACluster = false;
            for (Set<A> cluster: clusters) {
                boolean canGoInCluster = true;
                for (A otherSegmentInCluster: cluster) {
                    if (calcDistance(segment.getLat(), segment.getLong(), otherSegmentInCluster.getLat(), otherSegmentInCluster.getLong()) > length) {
                        canGoInCluster = false;
                        break;
                    }
                }
                if (canGoInCluster) {
                    cluster.add(segment);
                    hasGoneInACluster = true;
                    break;
                }
            }
            if (!hasGoneInACluster) {
                Set<A> cluster = new HashSet<A>();
                cluster.add(segment);
                clusters.add(cluster);
            }
        }
        return clusters;
    }

}