package net.emaze.networks;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;
import net.emaze.dysfunctional.contracts.dbc;
import net.emaze.dysfunctional.dispatching.delegates.Delegate;
import net.emaze.dysfunctional.order.CompareToBuilder;

public class SortCidrsByFirstThenLastIp implements Delegate<List<Cidr>, Collection<Cidr>> {

    @Override
    public List<Cidr> perform(Collection<Cidr> unsorted) {
        dbc.precondition(unsorted != null, "unsorted cannot be null");
        final List<Cidr> sorted = new ArrayList<>(unsorted);
        Collections.sort(sorted, new FirstIpThenLastIpCidrComparator());
        return sorted;
    }

    private static class FirstIpThenLastIpCidrComparator implements Comparator<Cidr> {

        @Override
        public int compare(Cidr lhs, Cidr rhs) {
            return new CompareToBuilder().append(lhs.firstIp(), rhs.firstIp()).append(lhs.lastIp(), rhs.lastIp()).toComparison();
        }
    }
}
