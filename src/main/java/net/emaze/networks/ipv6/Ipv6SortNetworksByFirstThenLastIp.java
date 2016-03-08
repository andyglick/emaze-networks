package net.emaze.networks.ipv6;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;
import java.util.function.Function;
import net.emaze.dysfunctional.contracts.dbc;
import net.emaze.dysfunctional.order.CompareToBuilder;

public class Ipv6SortNetworksByFirstThenLastIp implements Function<Collection<Ipv6Network>, List<Ipv6Network>> {

    @Override
    public List<Ipv6Network> apply(Collection<Ipv6Network> unsorted) {
        dbc.precondition(unsorted != null, "unsorted cannot be null");
        final List<Ipv6Network> sorted = new ArrayList<>(unsorted);
        Collections.sort(sorted, new FirstIpThenLastIpCidrComparator());
        return sorted;
    }

    private static class FirstIpThenLastIpCidrComparator implements Comparator<Ipv6Network> {

        @Override
        public int compare(Ipv6Network lhs, Ipv6Network rhs) {
            return new CompareToBuilder().append(lhs.firstIp(), rhs.firstIp()).append(lhs.lastIp(), rhs.lastIp()).toComparison();
        }
    }
}
