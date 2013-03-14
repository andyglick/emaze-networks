package net.emaze.networks;

import java.util.Collection;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Set;
import net.emaze.dysfunctional.Applications;
import net.emaze.dysfunctional.Compositions;
import net.emaze.dysfunctional.Consumers;
import net.emaze.dysfunctional.Multiplexing;
import net.emaze.dysfunctional.Tuples;
import net.emaze.dysfunctional.casts.Vary;
import net.emaze.dysfunctional.contracts.dbc;
import net.emaze.dysfunctional.dispatching.delegates.Delegate;
import net.emaze.dysfunctional.ranges.DenseRange;
import net.emaze.dysfunctional.ranges.Range;

public class DensifyNetworks implements Delegate<Set<Network>, Collection<Network>> {

    private static final Delegate<DenseRange<Ip>, Network> cidrToDenseRange = Compositions.compose(Tuples.tupled(new IpsToDenseRange()), new NetworkToIps());
    private static final Delegate<List<Network>, DenseRange<Ip>> denseRangeToCidr = Compositions.compose(Tuples.tupled(new IpRangeToNetworks()), new DenseRangeToIps());

    @Override
    public Set<Network> perform(Collection<Network> cidrs) {
        dbc.precondition(cidrs != null, "cidrs cannot be null");
        final Iterator<Range<Ip>> childrenAsRanges = Applications.transform(cidrs, Compositions.compose(new Vary<Range<Ip>, DenseRange<Ip>>(), cidrToDenseRange));
        final Range<Ip> union = new IpRanges().union(childrenAsRanges);
        final List<DenseRange<Ip>> densifiedUnion = union.densified();
        return Consumers.all(Multiplexing.flatten(Applications.transform(densifiedUnion, denseRangeToCidr)), new HashSet<Network>());
    }
}
