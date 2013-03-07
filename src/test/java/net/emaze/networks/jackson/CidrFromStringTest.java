package net.emaze.networks.jackson;

import com.fasterxml.jackson.databind.ObjectMapper;
import java.io.IOException;
import junit.framework.Assert;
import net.emaze.networks.Cidr;
import net.emaze.networks.jackson.CidrToStringTest.BeanWithCidr;
import org.junit.Test;

public class CidrFromStringTest {

    @Test
    public void deserializingYieldsExpected() throws IOException {
        final String serialized = "{'cidr':'10.0.0.0/8'}".replace("'", "\"");
        final Cidr expected = Cidr.parse("10.0.0.0/8");
        final ObjectMapper mapper = new ObjectMapper();
        mapper.registerModule(new NetworksModule());
        final BeanWithCidr got = mapper.readValue(serialized, BeanWithCidr.class);
        Assert.assertEquals(expected, got.getCidr());
    }
}
