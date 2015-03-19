package net.emaze.networks;

import java.util.Arrays;
import java.util.Collection;
import java.util.List;
import java.util.Set;

public class Ipv4NetworkOperations {

    public static Set<Ipv4Network> subtract(Ipv4Network minuend, Ipv4 subtrahend) {
        return new Ipv4SubtractIpFromNetwork().perform(minuend, subtrahend);
    }

    public static Set<Ipv4Network> subtract(Ipv4Network minuend, Ipv4Network... subtrahends) {
        return new Ipv4SubtractNetworksFromNetwork().perform(minuend, Arrays.asList(subtrahends));
    }

    public static Set<Ipv4Network> subtract(Ipv4Network minuend, Collection<Ipv4Network> subtrahends) {
        return new Ipv4SubtractNetworksFromNetwork().perform(minuend, subtrahends);
    }

    public static List<Ipv4Network> sortByFirstThenLastIp(Collection<Ipv4Network> cidrs) {
        return new Ipv4SortNetworksByFirstThenLastIp().perform(cidrs);
    }

    public static Set<Ipv4Network> densify(Ipv4Network... cidrs) {
        return new Ipv4DensifyNetworks().perform(Arrays.asList(cidrs));
    }

    public static Set<Ipv4Network> densify(Collection<Ipv4Network> cidrs) {
        return new Ipv4DensifyNetworks().perform(cidrs);
    }
}
