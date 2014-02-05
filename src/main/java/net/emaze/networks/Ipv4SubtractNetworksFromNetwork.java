package net.emaze.networks;

import java.util.Collection;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Set;
import net.emaze.dysfunctional.Applications;
import net.emaze.dysfunctional.Compositions;
import net.emaze.dysfunctional.Consumers;
import net.emaze.dysfunctional.Filtering;
import net.emaze.dysfunctional.Iterations;
import net.emaze.dysfunctional.Logic;
import net.emaze.dysfunctional.Multiplexing;
import net.emaze.dysfunctional.Tuples;
import net.emaze.dysfunctional.casts.Vary;
import net.emaze.dysfunctional.contracts.dbc;
import net.emaze.dysfunctional.dispatching.delegates.BinaryDelegate;
import net.emaze.dysfunctional.dispatching.delegates.Delegate;
import net.emaze.dysfunctional.ranges.DenseRange;
import net.emaze.dysfunctional.ranges.Range;
import net.emaze.dysfunctional.ranges.RangeIsEmpty;

public class Ipv4SubtractNetworksFromNetwork implements BinaryDelegate<Set<Ipv4Network>, Ipv4Network, Collection<Ipv4Network>> {

    private final Delegate<DenseRange<Ipv4>, Ipv4Network> cidrToDenseRange = Compositions.compose(Tuples.tupled(new Ipv4ToDenseRange()), new Ipv4NetworkToIp());
    private final Delegate<List<Ipv4Network>, DenseRange<Ipv4>> denseRangeToCidr = Compositions.compose(Tuples.tupled(new Ipv4RangeToNetworks()), new Ipv4DenseRangeToIp());

    @Override
    public Set<Ipv4Network> perform(Ipv4Network parent, Collection<Ipv4Network> children) {
        dbc.precondition(parent != null, "parent cannot be null");
        dbc.precondition(children != null, "children cannot be null");
        final Range<Ipv4> parentAsRange = cidrToDenseRange.perform(parent);
        final Iterator<Range<Ipv4>> childrenAsRanges = Applications.transform(children, Compositions.compose(new Vary<Range<Ipv4>, DenseRange<Ipv4>>(), cidrToDenseRange));
        final Range<Ipv4> remainder = IpRanges.RANGESV4.difference(Multiplexing.chain(Iterations.iterator(parentAsRange), childrenAsRanges));
        final Iterator<DenseRange<Ipv4>> densifiedNotEmptyRemainders = Filtering.filter(remainder.densified(), Logic.not(new RangeIsEmpty<DenseRange<Ipv4>, Ipv4>()));
        return Consumers.all(Multiplexing.flatten(Applications.transform(densifiedNotEmptyRemainders, denseRangeToCidr)), new HashSet<Ipv4Network>());
    }
}
