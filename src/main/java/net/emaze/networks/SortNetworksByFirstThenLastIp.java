package net.emaze.networks;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;
import net.emaze.dysfunctional.contracts.dbc;
import net.emaze.dysfunctional.dispatching.delegates.Delegate;
import net.emaze.dysfunctional.order.CompareToBuilder;

public class SortNetworksByFirstThenLastIp implements Delegate<List<Network>, Collection<Network>> {

    @Override
    public List<Network> perform(Collection<Network> unsorted) {
        dbc.precondition(unsorted != null, "unsorted cannot be null");
        final List<Network> sorted = new ArrayList<>(unsorted);
        Collections.sort(sorted, new FirstIpThenLastIpCidrComparator());
        return sorted;
    }

    private static class FirstIpThenLastIpCidrComparator implements Comparator<Network> {

        @Override
        public int compare(Network lhs, Network rhs) {
            return new CompareToBuilder().append(lhs.firstIp(), rhs.firstIp()).append(lhs.lastIp(), rhs.lastIp()).toComparison();
        }
    }
}
