package net.emaze.networks;

import java.util.Arrays;
import java.util.Collection;
import java.util.List;
import java.util.Set;

public abstract class NetworkOperations {

    public static Set<Network> subtract(Network minuend, Ip subtrahend) {
        return new SubtractIpFromNetwork().perform(minuend, subtrahend);
    }

    public static Set<Network> subtract(Network minuend, Network... subtrahends) {
        return new SubtractNetworksFromNetwork().perform(minuend, Arrays.asList(subtrahends));
    }

    public static List<Network> sortByFirstThenLastIp(Collection<Network> cidrs) {
        return new SortNetworksByFirstThenLastIp().perform(cidrs);
    }

    public static Set<Network> densify(Network... cidrs) {
        return new DensifyNetworks().perform(Arrays.asList(cidrs));
    }
}
