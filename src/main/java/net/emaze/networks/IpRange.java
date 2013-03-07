package net.emaze.networks;

import net.emaze.dysfunctional.Compositions;
import net.emaze.dysfunctional.Filtering;
import net.emaze.dysfunctional.Ranges;
import net.emaze.dysfunctional.dispatching.delegates.Delegate;
import net.emaze.dysfunctional.iterations.ConstantIterator;
import net.emaze.dysfunctional.options.Maybe;
import net.emaze.dysfunctional.order.ComparableComparator;
import net.emaze.dysfunctional.order.SequencingPolicy;
import net.emaze.dysfunctional.ranges.DenseRange;

public abstract class IpRange {

    private static final SequencingPolicy<Ipv4> forwardSequencingPolicy = new Ipv4ForwardSequencingPolicy();
    private static final SequencingPolicy<Ipv4> reverseSequencingPolicy = new Ipv4ReverseSequencingPolicy();
    private static final Ranges<Ipv4> RANGES = new Ranges<>(new ComparableComparator<Ipv4>(), forwardSequencingPolicy, Ipv4.FIRST_IP);

    public static Maybe<Ipv4> nextOf(Ipv4 ipv4) {
        return forwardSequencingPolicy.next(ipv4);
    }

    public static Maybe<Ipv4> previousOf(Ipv4 ipv4) {
        return reverseSequencingPolicy.next(ipv4);
    }

    public static Maybe<Ipv4> offset(Ipv4 ipv4, long offset) {
        final SequencingPolicy<Ipv4> policy = offset < 0 ? reverseSequencingPolicy : forwardSequencingPolicy;
        final Delegate<Maybe<Ipv4>, Maybe<Ipv4>> applyPolicyOffsetTimes = Compositions.compose(Filtering.take(Math.abs(offset), new ConstantIterator<Delegate<Maybe<Ipv4>, Maybe<Ipv4>>>(new ApplyPolicy<>(policy))));
        return applyPolicyOffsetTimes.perform(Maybe.just(ipv4));
    }

    public DenseRange<Ipv4> rangeOf(Ipv4 start, Ipv4 end) {
        return (DenseRange<Ipv4>) RANGES.closed(start, end);
    }
    
    public static class ApplyPolicy<T> implements Delegate<Maybe<T>, Maybe<T>> {

        private final SequencingPolicy<T> policy;

        public ApplyPolicy(SequencingPolicy<T> policy) {
            this.policy = policy;
        }

        @Override
        public Maybe<T> perform(Maybe<T> element) {
            return element.hasValue() ? policy.next(element.value()) : Maybe.<T>nothing();
        }
    }
}
