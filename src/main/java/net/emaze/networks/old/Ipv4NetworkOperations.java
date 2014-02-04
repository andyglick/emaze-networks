package net.emaze.networks.old;

import java.util.Arrays;
import java.util.Collection;
import java.util.List;
import java.util.Set;

public abstract class Ipv4NetworkOperations {

    public static Set<Ipv4Network> subtract(Ipv4Network minuend, Ipv4 subtrahend) {
        return new SubtractIpv4FromNetwork().perform(minuend, subtrahend);
    }

    public static Set<Ipv4Network> subtract(Ipv4Network minuend, Ipv4Network... subtrahends) {
        return new SubtractIpv4NetworksFromNetwork().perform(minuend, Arrays.asList(subtrahends));
    }

    public static Set<Ipv4Network> subtract(Ipv4Network minuend, List<Ipv4Network> subtrahends) {
        return new SubtractIpv4NetworksFromNetwork().perform(minuend, subtrahends);
    }

    public static List<Ipv4Network> sortByFirstThenLastIp(Collection<Ipv4Network> cidrs) {
        return new SortIpv4NetworksByFirstThenLastIp().perform(cidrs);
    }

    public static Set<Ipv4Network> densify(Ipv4Network... cidrs) {
        return new DensifyIpv4Networks().perform(Arrays.asList(cidrs));
    }
    
    public static Set<Ipv4Network> densify(Collection<Ipv4Network> cidrs) {
        return new DensifyIpv4Networks().perform(cidrs);
    }
}
