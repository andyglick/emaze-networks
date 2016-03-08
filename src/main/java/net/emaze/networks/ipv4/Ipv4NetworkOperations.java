package net.emaze.networks.ipv4;

import java.util.Arrays;
import java.util.Collection;
import java.util.List;
import java.util.Set;

public class Ipv4NetworkOperations {

    public static Set<Ipv4Network> subtract(Ipv4Network minuend, Ipv4 subtrahend) {
        return new Ipv4SubtractIpFromNetwork().apply(minuend, subtrahend);
    }

    public static Set<Ipv4Network> subtract(Ipv4Network minuend, Ipv4Network... subtrahends) {
        return new Ipv4SubtractNetworksFromNetwork().apply(minuend, Arrays.asList(subtrahends));
    }

    public static Set<Ipv4Network> subtract(Ipv4Network minuend, Collection<Ipv4Network> subtrahends) {
        return new Ipv4SubtractNetworksFromNetwork().apply(minuend, subtrahends);
    }

    public static List<Ipv4Network> sortByFirstThenLastIp(Collection<Ipv4Network> cidrs) {
        return new Ipv4SortNetworksByFirstThenLastIp().apply(cidrs);
    }

    public static Set<Ipv4Network> densify(Ipv4Network... cidrs) {
        return new Ipv4DensifyNetworks().apply(Arrays.asList(cidrs));
    }

    public static Set<Ipv4Network> densify(Collection<Ipv4Network> cidrs) {
        return new Ipv4DensifyNetworks().apply(cidrs);
    }
}
