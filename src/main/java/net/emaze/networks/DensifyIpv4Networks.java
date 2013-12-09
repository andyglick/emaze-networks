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

public class DensifyIpv4Networks implements Delegate<Set<Ipv4Network>, Collection<Ipv4Network>> {

    private static final Delegate<DenseRange<Ipv4>, Ipv4Network> cidrToDenseRange = Compositions.compose(Tuples.tupled(new Ipv4ToDenseRange()), new NetworkToIpv4());
    private static final Delegate<List<Ipv4Network>, DenseRange<Ipv4>> denseRangeToCidr = Compositions.compose(Tuples.tupled(new Ipv4RangeToNetworks()), new DenseRangeToIpv4());

    @Override
    public Set<Ipv4Network> perform(Collection<Ipv4Network> cidrs) {
        dbc.precondition(cidrs != null, "cidrs cannot be null");
        final Iterator<Range<Ipv4>> childrenAsRanges = Applications.transform(cidrs, Compositions.compose(new Vary<Range<Ipv4>, DenseRange<Ipv4>>(), cidrToDenseRange));
        final Range<Ipv4> union = new Ipv4Ranges().union(childrenAsRanges);
        final List<DenseRange<Ipv4>> densifiedUnion = union.densified();
        return Consumers.all(Multiplexing.flatten(Applications.transform(densifiedUnion, denseRangeToCidr)), new HashSet<Ipv4Network>());
    }
}
