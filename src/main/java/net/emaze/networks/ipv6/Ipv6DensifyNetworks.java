package net.emaze.networks.ipv6;

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
import net.emaze.networks.IpRanges;

public class Ipv6DensifyNetworks implements Delegate<Set<Ipv6Network>, Collection<Ipv6Network>> {

    private final Delegate<DenseRange<Ipv6>, Ipv6Network> cidrToDenseRange = Compositions.compose(Tuples.tupled(new Ipv6ToDenseRange()), new Ipv6NetworkToIp());
    private final Delegate<List<Ipv6Network>, DenseRange<Ipv6>> denseRangeToCidr = Compositions.compose(Tuples.tupled(new Ipv6RangeToNetworks()), new Ipv6DenseRangeToIp());

    @Override
    public Set<Ipv6Network> perform(Collection<Ipv6Network> cidrs) {
        dbc.precondition(cidrs != null, "cidrs cannot be null");
        final Iterator<Range<Ipv6>> childrenAsRanges = Applications.transform(cidrs, Compositions.compose(new Vary<Range<Ipv6>, DenseRange<Ipv6>>(), cidrToDenseRange));
        final Range<Ipv6> union = IpRanges.RANGESV6.union(childrenAsRanges);
        final List<DenseRange<Ipv6>> densifiedUnion = union.densified();
        return Consumers.all(Multiplexing.flatten(Applications.transform(densifiedUnion, denseRangeToCidr)), new HashSet<Ipv6Network>());
    }

}
