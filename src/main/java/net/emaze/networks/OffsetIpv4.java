package net.emaze.networks;

import net.emaze.dysfunctional.Compositions;
import net.emaze.dysfunctional.Filtering;
import net.emaze.dysfunctional.contracts.dbc;
import net.emaze.dysfunctional.dispatching.delegates.BinaryDelegate;
import net.emaze.dysfunctional.dispatching.delegates.Delegate;
import net.emaze.dysfunctional.iterations.ConstantIterator;
import net.emaze.dysfunctional.options.Maybe;
import net.emaze.dysfunctional.order.SequencingPolicy;

public class OffsetIpv4 implements BinaryDelegate<Maybe<Ipv4>, Ipv4, Long> {

    @Override
    public Maybe<Ipv4> perform(Ipv4 baseIp, Long offset) {
        dbc.precondition(baseIp != null, "base ip cannot be null");
        dbc.precondition(offset != null, "offset cannot be null");
        final SequencingPolicy<Ipv4> policy = offset < 0 ? new Ipv4ReverseSequencingPolicy() : new Ipv4ForwardSequencingPolicy();
        final Delegate<Maybe<Ipv4>, Maybe<Ipv4>> applyPolicyOffsetTimes = Compositions.compose(Filtering.take(Math.abs(offset), new ConstantIterator<Delegate<Maybe<Ipv4>, Maybe<Ipv4>>>(new ApplyPolicy<>(policy))));
        return applyPolicyOffsetTimes.perform(Maybe.just(baseIp));
    }
}
