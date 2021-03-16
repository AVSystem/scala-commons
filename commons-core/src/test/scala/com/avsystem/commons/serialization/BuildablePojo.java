package com.avsystem.commons.serialization;

import java.util.ArrayList;
import java.util.List;
import java.util.Objects;

public final class BuildablePojo {
    private final String str;
    private final int num;
    private final List<Boolean> flags;
    private final boolean cool;

    private BuildablePojo(String str, int num, List<Boolean> flags, boolean cool) {
        this.str = str;
        this.num = num;
        this.flags = flags;
        this.cool = cool;
    }

    public String getStr() {
        return str;
    }

    public int getNum() {
        return num;
    }

    public List<Boolean> getFlags() {
        return flags;
    }

    public boolean isCool() {
        return cool;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        BuildablePojo that = (BuildablePojo) o;
        return num == that.num && cool == that.cool && Objects.equals(str, that.str) && Objects.equals(flags, that.flags);
    }

    @Override
    public int hashCode() {
        return Objects.hash(str, num, flags, cool);
    }

    public static Builder builder() {
        return new Builder();
    }

    public static class Builder {
        private String str = null;
        private int num = 0;
        private List<Boolean> flags = new ArrayList<>();
        private boolean cool = true;

        private Builder() {
        }

        public Builder setStr(String str) {
            this.str = str;
            return this;
        }

        public Builder setNum(int num) {
            this.num = num;
            return this;
        }

        public Builder setFlags(List<Boolean> flags) {
            this.flags = flags;
            return this;
        }

        public Builder setCool(boolean cool) {
            this.cool = cool;
            return this;
        }

        public BuildablePojo build() {
            return new BuildablePojo(str, num, flags, cool);
        }
    }
}
