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
import net.emaze.dysfunctional.Reductions;
import net.emaze.dysfunctional.Tuples;
import net.emaze.dysfunctional.casts.Vary;
import net.emaze.dysfunctional.contracts.dbc;
import net.emaze.dysfunctional.dispatching.delegates.BinaryDelegate;
import net.emaze.dysfunctional.dispatching.delegates.Delegate;
import net.emaze.dysfunctional.ranges.DenseRange;
import net.emaze.dysfunctional.ranges.Range;

public class SubtractCidrsFromCidr implements BinaryDelegate<Set<Cidr>, Cidr, Collection<Cidr>> {

    final Delegate<DenseRange<Ipv4>, Cidr> cidrToDenseRange = Compositions.compose(Tuples.tupled(new IpsToDenseRange()), new CidrToIps());
    final Delegate<List<Cidr>, DenseRange<Ipv4>> denseRangeToCidr = Compositions.compose(Tuples.tupled(new IpRangeToCidrs()), new DenseRangeToIps());

    @Override
    public Set<Cidr> perform(Cidr parent, Collection<Cidr> children) {
        dbc.precondition(parent != null, "parent cannot be null");
        dbc.precondition(children != null, "children cannot be null");
        final Range<Ipv4> parentAsRange = cidrToDenseRange.perform(parent);
        final Iterator<Range<Ipv4>> childrenAsRanges = Applications.transform(children, Compositions.compose(new Vary<Range<Ipv4>, DenseRange<Ipv4>>(), cidrToDenseRange));
        final Range<Ipv4> remainder = Reductions.reduce(childrenAsRanges, new SubtractRangeFromRange(), (Range) parentAsRange);
        final List<DenseRange<Ipv4>> densifiedRemainder = remainder.densified();
        return Consumers.all(Multiplexing.flatten(Applications.transform(densifiedRemainder, denseRangeToCidr)), new HashSet<Cidr>());
    }
}
