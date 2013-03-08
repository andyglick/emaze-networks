package net.emaze.networks;

import java.util.Arrays;
import java.util.Collection;
import java.util.List;
import java.util.Set;

public abstract class CidrOperations {

    public static Set<Cidr> subtract(Cidr minuend, Ipv4 subtrahend) {
        return new SubtractIpFromCidr().perform(minuend, subtrahend);
    }

    public static Set<Cidr> subtract(Cidr minuend, Cidr... subtrahends) {
        return new SubtractCidrsFromCidr().perform(minuend, Arrays.asList(subtrahends));
    }

    public static List<Cidr> sortByFirstThenLastIp(Collection<Cidr> cidrs) {
        return new SortCidrsByFirstThenLastIp().perform(cidrs);
    }

    public static Set<Cidr> densify(Cidr... cidrs) {
        return new DensifyCidrs().perform(Arrays.asList(cidrs));
    }
}
