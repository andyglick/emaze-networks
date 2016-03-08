package net.emaze.networks.ipv6;

import java.util.Arrays;
import java.util.Collection;
import java.util.List;
import java.util.Set;

public class Ipv6NetworkOperations {

    public static Set<Ipv6Network> subtract(Ipv6Network minuend, Ipv6 subtrahend) {
        return new Ipv6SubtractIpFromNetwork().apply(minuend, subtrahend);
    }

    public static Set<Ipv6Network> subtract(Ipv6Network minuend, Ipv6Network... subtrahends) {
        return new Ipv6SubtractNetworksFromNetwork().apply(minuend, Arrays.asList(subtrahends));
    }

    public static Set<Ipv6Network> subtract(Ipv6Network minuend, Collection<Ipv6Network> subtrahends) {
        return new Ipv6SubtractNetworksFromNetwork().apply(minuend, subtrahends);
    }

    public static List<Ipv6Network> sortByFirstThenLastIp(Collection<Ipv6Network> cidrs) {
        return new Ipv6SortNetworksByFirstThenLastIp().apply(cidrs);
    }

    public static Set<Ipv6Network> densify(Ipv6Network... cidrs) {
        return new Ipv6DensifyNetworks().apply(Arrays.asList(cidrs));
    }

    public static Set<Ipv6Network> densify(Collection<Ipv6Network> cidrs) {
        return new Ipv6DensifyNetworks().apply(cidrs);
    }
}
