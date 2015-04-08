package net.emaze.networks;

import java.util.Arrays;
import java.util.Collection;
import java.util.List;
import java.util.Set;

public class Ipv6NetworkOperations {

    public static Set<Ipv6Network> subtract(Ipv6Network minuend, Ipv6 subtrahend) {
        return new Ipv6SubtractIpFromNetwork().perform(minuend, subtrahend);
    }

    public static Set<Ipv6Network> subtract(Ipv6Network minuend, Ipv6Network... subtrahends) {
        return new Ipv6SubtractNetworksFromNetwork().perform(minuend, Arrays.asList(subtrahends));
    }

    public static Set<Ipv6Network> subtract(Ipv6Network minuend, Collection<Ipv6Network> subtrahends) {
        return new Ipv6SubtractNetworksFromNetwork().perform(minuend, subtrahends);
    }

    public static List<Ipv6Network> sortByFirstThenLastIp(Collection<Ipv6Network> cidrs) {
        return new Ipv6SortNetworksByFirstThenLastIp().perform(cidrs);
    }

    public static Set<Ipv6Network> densify(Ipv6Network... cidrs) {
        return new Ipv6DensifyNetworks().perform(Arrays.asList(cidrs));
    }

    public static Set<Ipv6Network> densify(Collection<Ipv6Network> cidrs) {
        return new Ipv6DensifyNetworks().perform(cidrs);
    }
}
