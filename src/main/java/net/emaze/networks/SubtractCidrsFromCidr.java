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

public class SubtractCidrsFromCidr implements BinaryDelegate<Set<Cidr>, Cidr, Collection<Cidr>> {

    private static final Delegate<DenseRange<Ip>, Cidr> cidrToDenseRange = Compositions.compose(Tuples.tupled(new IpsToDenseRange()), new CidrToIps());
    private static final Delegate<List<Cidr>, DenseRange<Ip>> denseRangeToCidr = Compositions.compose(Tuples.tupled(new IpRangeToCidrs()), new DenseRangeToIps());

    @Override
    public Set<Cidr> perform(Cidr parent, Collection<Cidr> children) {
        dbc.precondition(parent != null, "parent cannot be null");
        dbc.precondition(children != null, "children cannot be null");
        final Range<Ip> parentAsRange = cidrToDenseRange.perform(parent);
        final Iterator<Range<Ip>> childrenAsRanges = Applications.transform(children, Compositions.compose(new Vary<Range<Ip>, DenseRange<Ip>>(), cidrToDenseRange));
        final Range<Ip> remainder = new Ipv4Ranges().difference(Multiplexing.chain(Iterations.iterator(parentAsRange), childrenAsRanges));
        //Reductions.reduce(childrenAsRanges, new SubtractRangeFromRange(), (Range) parentAsRange);
        final Iterator<DenseRange<Ip>> densifiedNotEmptyRemainders = Filtering.filter(remainder.densified(), Logic.not(new RangeIsEmpty<DenseRange<Ip>, Ip>()));
        return Consumers.all(Multiplexing.flatten(Applications.transform(densifiedNotEmptyRemainders, denseRangeToCidr)), new HashSet<Cidr>());
    }
}
